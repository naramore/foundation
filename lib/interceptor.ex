defmodule Interceptor do
  @moduledoc """
  Inspired by: https://github.com/exoscale/interceptor

  An interceptor is a map or map-like object with the keys
  `:enter`, `:leave`, and `:error`. The value of each key is a
  function; missing keys or `nil` values are ignored.

  When executing a context, first all the `:enter` functions are
  invoked in order. As this happens, the interceptors are pushed
  onto a stack.

  When execution reaches the end of the queue, it begins popping
  interceptors off the stack and calling their `:leave` functions.
  Therefore `:leave` functions are called in the opposite order
  from `:enter` functions.

  For example:

  ```
  enter A -> enter B -> enter C -> leave C -> leave B -> leave A
  ```

  Both the `:enter` and `:leave` functions are called on a single
  argument, the context map, and return an updated context.

  If any interceptor function throws an exception, execution stops
  and begins popping interceptors off the stack and calling their
  `:error` functions. The `:error` function takes 2 arguments:
  the context and the error triggering the call.

  For example:

  ```
  enter A -> enter B -> enter C -> leave C -> leave B -> leave A
                          |
                        (error)
                          |
  error A <- error B <----+
  ```
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
  The context contains 3 key pieces of data:

  - `:__queue__` - a `t:queue.queue()` of interceptor stages.

  - `:__stack__` - a list of interceptor stages.

  - `:__status__` - one of `:enter`, `:leave`, or `{:error, _}`.

  With the rest of the map being arbitrary and specific to the
  domain of the interceptor stages.
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

  #############################################################################
  # region API
  #############################################################################

  @doc """
  Similar to `execute(ctx, [], [])`.

  See `execute/3` for more details.
  """
  @spec execute(ctx()) :: {:ok, ctx()} | {:error, err()}
  def execute(ctx) do
    case execute_impl(ctx) do
      %{__status__: {:error, reason}} -> {:error, reason}
      ctx -> {:ok, ctx}
    end
  end

  @doc """
  Executes a queue of interceptors attached to the context.

  ## Options

  * `:debug?` - enables stage logging (defaults to `false`).

  See `debug_stages/2` for more details on logging options.
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
  Adds an interceptor stage to the given `ctx`.
  """
  @spec stage(ctx(), Stage.name(), keyword()) :: ctx()
  def stage(ctx, name, opts \\ []) do
    enqueue(ctx, [Stage.create(name, opts)])
  end

  @doc """
  Initializes the context with values from `ctx`.
  """
  @spec start(map()) :: ctx()
  def start(ctx) do
    Map.merge(new(), ctx)
  end

  @doc """
  Creates an empty context.
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
  Applies logging to chain of interceptors.

  ## Options

  * `:keys` - the keys to be included in the log,
    see `Logging Keys` section for more details
    (defaults to `[:where, :stage, :ctx, :error]`).

  * `:stages` - the interceptor stages logging will be applied to
    (defaults to `[:enter, :leave, :error]`).

  * `:clean_ctx?` - whether or not to run `clean/1` against the context
    prior to logging (defaults to `true`).

  * `:before?` - log before interceptor stage (defaults to `true`).

  * `:after?` - log after interceptor stage (defaults to `true`).

  ## Logging Keys

  * `:where` - either `:before` or `:after`.

  * `:stage` - a tuple of the interceptor name and stage
    (e.g. `{:foo, :enter}`).

  * `:ctx` - the full (or cleaned) context.

  * `:error` - the current error (should be `nil` for non-`:error` stages).
  """
  @spec debug_stages([stage()], keyword()) :: [stage()]
  def debug_stages(chain, opts \\ []) do
    stages = Keyword.get(opts, :stages, @default_stages)
    log_keys = Keyword.get(opts, :keys, @default_keys)
    clean_ctx? = Keyword.get(opts, :clean_ctx?, true)

    into_stages(chain, stages, fn sf, %{interceptor: i, stage: k} ->
      sf
      |> then(
        &if Keyword.get(opts, :after?, true) do
          after_stage(&1, do_debug(:after, i, k, log_keys, clean_ctx?))
        end
      )
      |> then(
        &if Keyword.get(opts, :before?, true) do
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
  Removes all remaining interceptors from context's execution queue.
  This effectively short-circuits execution of interceptors' `:enter`
  functions and begins executing the `:leave` functions.
  """
  @spec terminate(ctx()) :: ctx()
  def terminate(ctx) do
    Map.put(ctx, :__queue__, :queue.new())
  end

  @doc """
  Removes all remaining interceptors from context's execution queue and stack.
  This effectively short-circuits execution of
  interceptors' `:enter`/`:leave` and returns the context.
  """
  @spec halt(ctx()) :: ctx()
  def halt(ctx) do
    ctx
    |> terminate()
    |> Map.put(:__stack__, [])
  end

  @doc """
  Adds interceptors to current context.
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
  Takes a context from execution and run `xf` (transducing fun) on stack,
  returns a new context.
  """
  @spec xform_stack(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform_stack(ctx, xf) do
    Map.update!(ctx, :__stack__, &Duct.into(&1, xf))
  end

  @doc """
  Takes a context from execution and run `xf` (transducing fun) on queue,
  returns a new context.
  """
  @spec xform_queue(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform_queue(ctx, xf) do
    Map.update!(ctx, :__queue__, fn q ->
      q |> :queue.to_list() |> Duct.into(xf) |> :queue.from_list()
    end)
  end

  @doc """
  Takes a context from execution and run `xf` (transducing fun) on stack/queue,
  returns a new context.
  """
  @spec xform(ctx(), Duct.transducer(stage(), any(), stage(), any())) :: ctx()
  def xform(ctx, xf) do
    ctx
    |> xform_queue(xf)
    |> xform_stack(xf)
  end

  @doc """
  Remove all interceptors matching `pred` from stack/queue,
  returns context.
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
  Takes a stage function, and wraps it with a callback that will
  return a new context from the application of `g` onto it. It can be
  useful to run a separate function after a stage returns and apply
  some transformation to it relative to the original context.
  """
  @spec transform((a -> b), (ctx(), b -> c)) :: (a -> c) when a: any, b: any, c: any
  def transform(f, g) do
    fn ctx -> g.(ctx, f.(ctx)) end
  end

  @doc """
  Modifies interceptor stage to *take in* specified `path`.
  """
  @spec take_in(stage_fun(), path :: [any, ...]) :: stage_fun()
  def take_in(f, path) do
    fn ctx -> f.(get_in(ctx, path)) end
  end

  @doc """
  Modifies interceptor stage to *return at* specified `path`.
  """
  @spec return_at(stage_fun(), path :: [any, ...]) :: stage_fun()
  def return_at(f, path) do
    transform(f, &put_in(&1, path, &2))
  end

  @doc """
  Modifies interceptor stage to only run on `ctx` if `pred`
  returns `true`.
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
  Modifies interceptor stage to take from `path` and return to `path`.
  """
  @spec lens(stage_fun(), path :: [any, ...]) :: stage_fun()
  def lens(f, path) do
    f
    |> take_in(path)
    |> return_at(path)
  end

  @doc """
  Run function for side-effects only and return context.
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
  Wraps stage fn with another one, basically a middleware
  that will be run before a stage.
  """
  @spec before_stage(fun(), fun()) :: fun()
  def before_stage(stage, before_stage) do
    compose_stage(stage, before_stage)
  end

  @doc """
  Modifies context after stage function ran.
  """
  @spec after_stage(fun(), fun()) :: fun()
  def after_stage(stage, after_stage) do
    compose_stage(after_stage, stage)
  end

  @doc """
  Applies function `f` to all `stages` of `chain`. This provides a way to
  apply middleware to an entire interceptor chain at definition time.

  ## Note

  Useful when used in conjunction with, `after_stage/2`, `before_stage/2`.

  ## Example

    iex> alias Interceptor.Stage
    iex> e = fn k, v -> &Map.update(&1, k, [v], &(&1 ++ [v])) end
    iex> stages = [
    ...>    Stage.create(:a, enter: e.(:x, :a), leave: e.(:x, :f)),
    ...>    Stage.create(:b, enter: e.(:x, :b), leave: e.(:x, :e)),
    ...>    Stage.create(:c, enter: e.(:x, :c), leave: e.(:x, :d)),
    ...> ]
    iex> stages = into_stages(stages, [:enter, :leave], fn stage_f, _ctx ->
    ...>   after_stage(stage_f, &Map.update(&1, :cnt, 1, fn x -> x + 1 end))
    ...> end)
    iex> Interceptor.start(%{})
    ...> |> Interceptor.execute(stages)
    ...> |> Interceptor.clean()
    %{x: [:a, :b, :c, :d, :e, :f], cnt: 6}
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
