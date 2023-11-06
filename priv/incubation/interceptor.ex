defmodule Interceptor do
  @moduledoc """
  """

  alias Interceptor.Stage

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
  @spec execute(ctx(), [stage()]) :: {:ok, ctx()} | {:error, err()}
  def execute(ctx, stages) do
    ctx
    |> start()
    |> enqueue(stages)
    |> execute()
  end

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
    %{ctx | __queue__: :queue.cons(x, q), stack: xs}
    |> then(&Stage.invoke(x, &1))
    |> execute_impl()
  end

  defp execute_impl(%{__stack__: [], __status__: {:error, _}} = ctx) do
    ctx
  end

  defp execute_impl(%{__queue__: q, __stack__: [x | xs], __status__: {:error, _}} = ctx) do
    %{ctx | __queue__: :queue.cons(x, q), stack: xs}
    |> then(&Stage.invoke(x, &1))
    |> execute_impl()
  end

  @doc """
  """
  @spec stage(ctx(), keyword()) :: ctx()
  def stage(ctx, opts) do
    enqueue(ctx, [Stage.create(opts)])
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
  """
  @spec error(ctx(), err()) :: ctx()
  def error(ctx, err) do
    Map.put(ctx, :__status__, {:error, err})
  end

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
  def enqueue(ctx, interceptors) do
    Map.update(
      ctx,
      :__queue__,
      :queue.from_list(interceptors),
      fn q ->
        Enum.reduce(interceptors, q, &:queue.in/2)
      end
    )
  end

  # @spec xform_stack()
  # @spec xform_queue()
  # @spec xform()
  # @spec remove()

  @doc """
  """
  @spec transform((a -> b), (ctx(), b -> c)) :: (a -> c) when a: any, b: any, c: any
  def transform(f, g) do
    fn ctx -> g.(ctx, f.(ctx)) end
  end

  @doc """
  """
  @spec take_in(stage_fun(), path :: [any, ...]) :: stage_fun()
  def take_in(f, path) do
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
    |> take_in(path)
    |> out(path)
  end

  @doc """
  """
  @spec discard(stage_fun()) :: stage_fun()
  def discard(f) do
    transform(f, fn ctx, _ -> ctx end)
  end

  # @spec before_stage(sef(), sef()) :: sef()
  # @spec after_stage(sef(), sef()) :: sef()
  # @spec into_stages()
end
