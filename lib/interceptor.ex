defmodule Interceptor do
  @moduledoc """
  https://github.com/exoscale/interceptor
  """
  use Boundary, top_level?: true, deps: [Duct], exports: [Stage]

  require Logger

  alias Interceptor.Stage

  @default_stages [:enter, :leave, :error]
  @default_keys [:where, :stage, :ctx, :error]

  @type stage :: Stage.t()
  @type err :: Exception.t() | (reason :: term)
  @type status :: :enter | :leave | {:error, err()}

  @typedoc """
  """
  @type ctx :: %{
          :__queue__ => :queue.queue(),
          :__stack__ => list(),
          :__status__ => status(),
          optional(any) => any
        }

  @type stage_fun :: Stage.stage_fun()
  @type error_fun :: Stage.error_fun()
  @type maybe(x) :: x | nil

  # TODO: remove this + replace w/ tests
  @doc false
  def test do
    inc =
      Stage.create(:inc,
        enter: &inc_ctx(&1, :a),
        leave: &inc_ctx(&1, :b)
      )

    a =
      Stage.create(:a,
        enter: &conj_ctx(&1, :x, :a),
        leave: &conj_ctx(&1, :x, :f)
      )

    b =
      Stage.create(:b,
        enter: &conj_ctx(&1, :x, :b),
        leave: &conj_ctx(&1, :x, :e)
      )

    c =
      Stage.create(:c,
        enter: &conj_ctx(&1, :x, :c),
        leave: &conj_ctx(&1, :x, :d)
      )

    wtf =
      Stage.create(:wtf,
        enter: &conj_ctx(&1, :x, :wtf),
        leave: fn c ->
          c
          |> conj_ctx(:x, :wtf_out!)
          |> enqueue([a])
        end
      )

    [a, inc, b, wtf, inc, c]
    |> then(&execute(%{a: 0, b: 0}, &1, debug?: true))
  end

  defp conj_ctx(ctx, key, val) do
    Map.update(ctx, key, [val], &(&1 ++ [val]))
  end

  defp inc_ctx(ctx, key) do
    Map.update(ctx, key, 1, &(&1 + 1))
  end

  #############################################################################
  # region API
  #############################################################################

  @doc """
  """
  @spec execute(ctx()) :: {:ok, ctx()} | {:error, err()}
  def execute(ctx) do
    case execute_impl(ctx) do
      %{__status__: {:error, reason}} -> {:error, reason}
      ctx -> {:ok, ctx}
    end
  end

  @doc """
  """
  @spec execute(map(), [stage()], keyword()) :: {:ok, ctx()} | {:error, err()}
  def execute(ctx, stages, opts \\ []) do
    ctx
    |> start()
    |> enqueue(stages)
    |> then(
      &if Keyword.get(opts, :debug?, false) do
        # TODO: clean this up w/ xform_queue or make something else that does it?
        #       (perhaps create into_stage/3? a version that returns a transducer?)
        &1.__queue__
        |> :queue.to_list()
        |> debug_stages(opts)
        |> :queue.from_list()
        |> then(fn ss -> Map.put(&1, :__queue__, ss) end)
      else
        &1
      end
    )
    |> execute()
  end

  @doc """
  """
  @spec stage(ctx(), Stage.name(), keyword()) :: ctx()
  def stage(ctx, name, opts \\ []) do
    enqueue(ctx, [Stage.create(name, opts)])
  end

  @doc """
  """
  @spec start(map()) :: ctx()
  def start(ctx) do
    Map.merge(new(), ctx)
  end

  @doc """
  """
  @spec new() :: ctx()
  def new do
    %{
      __queue__: :queue.new(),
      __stack__: [],
      __status__: :enter
    }
  end

  @doc """
  Removes interceptor-specific logic from a map (e.g. context).
  """
  @spec clean(map()) :: map()
  def clean(ctx) do
    {_, data} = Map.split(ctx, [:__queue__, :__stack__, :__status__])
    data
  end

  @doc """
  """
  @spec debug_stages([stage()], keyword()) :: [stage()]
  def debug_stages(chain, opts \\ []) do
    stages = Keyword.get(opts, :stages, @default_stages)
    log_keys = Keyword.get(opts, :keys, @default_keys)
    clean_ctx? = Keyword.get(opts, :clean_ctx?, true)

    into_stages(chain, stages, fn sf, %{interceptor: i, stage: k} ->
      sf
      |> then(
        &if Keyword.get(opts, :after, true) do
          after_stage(&1, do_debug(:after, i, k, log_keys, clean_ctx?))
        end
      )
      |> then(
        &if Keyword.get(opts, :before, true) do
          before_stage(&1, do_debug(:before, i, k, log_keys, clean_ctx?))
        end
      )
    end)
  end

  # endregion API

  #############################################################################
  # region Error Handling
  #############################################################################

  @doc """
  Adds error to context, potentially triggering `:error` stage on current/next
  interceptor.
  """
  @spec error(ctx(), err()) :: ctx()
  def error(ctx, err) do
    Map.put(ctx, :__status__, {:error, err})
  end

  @doc """
  Sets interceptor status to `:enter`, potentially resetting the execution
  "direction".
  """
  @spec enter(ctx()) :: ctx()
  def enter(ctx) do
    Map.put(ctx, :__status__, :enter)
  end

  @doc """
  Sets interceptor status to `:leave`, potentially skipping any remaining
  interceptor stages remaining in the queue.
  """
  @spec leave(ctx()) :: ctx()
  def leave(ctx) do
    Map.put(ctx, :__status__, :leave)
  end

  # endregion Error Handling

  #############################################################################
  # region Queue / Stack Manipulation
  #############################################################################

  @doc """
  """
  @spec terminate(ctx()) :: ctx()
  def terminate(ctx) do
    Map.put(ctx, :__queue__, :queue.new())
  end

  @doc """
  """
  @spec halt(ctx()) :: ctx()
  def halt(ctx) do
    ctx
    |> terminate()
    |> Map.put(:__stack__, [])
  end

  @doc """
  """
  @spec enqueue(ctx(), [stage()]) :: ctx()
  def enqueue(ctx, interceptors)

  def enqueue(%{__status__: :enter} = ctx, interceptors) do
    Map.update(
      ctx,
      :__queue__,
      :queue.from_list(interceptors),
      fn q ->
        Enum.reduce(interceptors, q, &:queue.in/2)
      end
    )
  end

  def enqueue(ctx, interceptors) do
    Map.update(ctx, :__stack__, interceptors, &(interceptors ++ &1))
  end

  @doc """
  """
  @spec xform_stack(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform_stack(ctx, xf) do
    Map.update!(ctx, :__stack__, &Duct.into(&1, xf))
  end

  @doc """
  """
  @spec xform_queue(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform_queue(ctx, xf) do
    Map.update!(ctx, :__queue__, fn q ->
      q |> :queue.to_list() |> Duct.into(xf) |> :queue.from_list()
    end)
  end

  @doc """
  """
  @spec xform(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform(ctx, xf) do
    ctx
    |> xform_queue(xf)
    |> xform_stack(xf)
  end

  @doc """
  """
  @spec remove(ctx(), (stage() -> boolean())) :: ctx()
  def remove(ctx, pred) do
    xform(ctx, Duct.reject(pred))
  end

  # endregion Queue / Stack Manipulation

  #############################################################################
  # region Helpers / Middleware
  #############################################################################

  @doc """
  """
  @spec transform((a -> b), (ctx(), b -> c)) :: (a -> c) when a: any, b: any, c: any
  def transform(f, g) do
    fn ctx -> g.(ctx, f.(ctx)) end
  end

  @doc """
  """
  @spec into(stage_fun(), path :: [any, ...]) :: stage_fun()
  def into(f, path) do
    fn ctx -> f.(get_in(ctx, path)) end
  end

  @doc """
  """
  @spec out(stage_fun(), path :: [any, ...]) :: stage_fun()
  def out(f, path) do
    transform(f, &put_in(&1, path, &2))
  end

  @doc """
  """
  @spec whenever(stage_fun(), (ctx() -> boolean)) :: stage_fun()
  def whenever(f, pred) do
    fn ctx ->
      if pred.(ctx) do
        f.(ctx)
      else
        ctx
      end
    end
  end

  @doc """
  """
  @spec lens(stage_fun(), path :: [any, ...]) :: stage_fun()
  def lens(f, path) do
    f
    |> into(path)
    |> out(path)
  end

  @doc """
  """
  @spec discard(stage_fun()) :: stage_fun()
  def discard(f) do
    transform(f, fn ctx, _ -> ctx end)
  end

  # endregion Helpers / Middleware

  #############################################################################
  # region Stage Middleware
  #############################################################################

  @doc """
  """
  @spec compose_stage(fun(), fun()) :: fun()
  def compose_stage(f, g) when is_function(f, 1) do
    fn x -> f.(g.(x)) end
  end

  def compose_stage(f, g) when is_function(f, 2) do
    fn x, y -> f.(g.(x, y), y) end
  end

  @doc """
  """
  @spec before_stage(fun(), fun()) :: fun()
  def before_stage(stage, before_stage) do
    compose_stage(stage, before_stage)
  end

  @doc """
  """
  @spec after_stage(fun(), fun()) :: fun()
  def after_stage(stage, after_stage) do
    compose_stage(after_stage, stage)
  end

  @doc """
  """
  @spec into_stages([stage()], [atom()], (fun(), map() -> fun())) :: [stage()]
  def into_stages(chain, stages \\ @default_stages, f) do
    Enum.map(chain, fn i ->
      Enum.reduce(stages, i, fn k, i ->
        stage = Map.get(i, k)

        if is_nil(stage) do
          i
        else
          Map.put(i, k, f.(stage, %{interceptor: i, stage: k}))
        end
      end)
    end)
  end

  # endregion Stage Middleware

  #############################################################################
  # region Private Implementation
  #############################################################################

  @spec execute_impl(ctx()) :: ctx()
  defp execute_impl(%{__queue__: q, __stack__: s, __status__: :enter} = ctx) do
    case :queue.out(q) do
      {{:value, x}, q} ->
        %{ctx | __queue__: q, __stack__: [x | s]}
        |> then(&Stage.invoke(x, &1))
        |> execute_impl()

      _ ->
        execute_impl(%{ctx | __status__: :leave})
    end
  end

  defp execute_impl(%{__stack__: [], __status__: :leave} = ctx) do
    ctx
  end

  defp execute_impl(%{__queue__: q, __stack__: [x | xs], __status__: :leave} = ctx) do
    %{ctx | __queue__: :queue.cons(x, q), __stack__: xs}
    |> then(&Stage.invoke(x, &1))
    |> execute_impl()
  end

  defp execute_impl(%{__stack__: [], __status__: {:error, _}} = ctx) do
    ctx
  end

  defp execute_impl(%{__queue__: q, __stack__: [x | xs], __status__: {:error, _}} = ctx) do
    %{ctx | __queue__: :queue.cons(x, q), __stack__: xs}
    |> then(&Stage.invoke(x, &1))
    |> execute_impl()
  end

  @spec do_debug(atom(), stage(), atom(), [atom()], boolean()) :: fun()
  defp do_debug(where, i, :error, keys, clean?) do
    fn ctx, err ->
      tap(
        ctx,
        &Logger.warning(fn ->
          build_debug(&1, err, where, i, :error, keys, clean?)
        end)
      )
    end
  end

  defp do_debug(where, i, k, keys, clean?) do
    fn ctx ->
      tap(
        ctx,
        &Logger.debug(fn ->
          build_debug(&1, nil, where, i, k, keys, clean?)
        end)
      )
    end
  end

  @spec build_debug(ctx(), err(), atom(), stage(), atom(), [atom()], boolean()) :: map()
  defp build_debug(ctx, err, where, i, k, keys, clean?) do
    ctx
    |> then(&if clean?, do: clean(&1), else: &1)
    |> then(&%{where: where, stage: {i.name, k}, ctx: &1, error: err})
    |> Enum.filter(fn {k, _} -> k in keys end)
    |> Enum.into(%{})
  end

  # endregion Private Implementation
end
