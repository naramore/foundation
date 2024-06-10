defmodule Interceptor.Stage do
  @moduledoc """
  """

  defstruct name: nil,
            module: nil,
            enter: nil,
            leave: nil,
            error: nil

  @type t :: %__MODULE__{
          name: name(),
          module: module() | nil,
          enter: stage_fun(),
          leave: stage_fun(),
          error: error_fun()
        }

  @type name :: term()
  @type ctx :: Interceptor.ctx()
  @type err :: Interceptor.err()
  @type stage_fun :: (ctx() -> ctx())
  @type error_fun :: (ctx(), err() -> ctx())
  @type opt ::
          {:module, module() | nil}
          | {:enter, stage_fun() | nil}
          | {:leave, stage_fun() | nil}
          | {:error, error_fun() | nil}

  @doc """
  """
  @callback enter(ctx()) :: ctx()

  @doc """
  """
  @callback leave(ctx()) :: ctx()

  @doc """
  """
  @callback error(ctx(), err()) :: ctx()

  @doc """
  """
  @spec new(name(), [opt()]) :: t()
  def new(name, opts \\ []) do
    new(
      name,
      Keyword.get(opts, :module),
      Keyword.get(opts, :enter, &default/1),
      Keyword.get(opts, :leave, &default/1),
      Keyword.get(opts, :error, &default_error/2)
    )
  end

  @doc false
  @spec new(name(), module() | nil, stage_fun() | nil, stage_fun() | nil, error_fun() | nil) ::
          t()
  def new(name, module, enter, leave, error) do
    %__MODULE__{
      name: name,
      module: module,
      enter: enter,
      leave: leave,
      error: error
    }
  end

  @doc """
  """
  @spec create(name(), keyword()) :: t() | nil
  def create(name, opts \\ []) do
    cond do
      Keyword.has_key?(opts, :module) ->
        module = Keyword.get(opts, :module)

        new(
          name,
          module: Keyword.get(opts, :module),
          enter: &module.enter/1,
          leave: &module.leave/1,
          error: &module.error/2
        )

      Keyword.has_key?(opts, :map) ->
        opts
        |> Keyword.get(:map)
        |> Enum.into([])
        |> then(&new(name, &1))

      true ->
        new(name, opts)
    end
  end

  @doc """
  """
  @spec invoke(t(), ctx()) :: ctx()
  def invoke(stage, ctx) do
    case {stage, ctx} do
      {%{enter: e}, %{__status__: :enter}} when is_function(e, 1) ->
        e.(ctx)

      {%{leave: l}, %{__status__: :leave}} when is_function(l, 1) ->
        l.(ctx)

      {%{error: e}, %{__status__: {:error, err}}} when is_function(e, 2) ->
        e.(ctx, err)

      {_, %{__status__: {:error, err}}} ->
        Interceptor.error(ctx, err)

      _ ->
        ctx
    end
  rescue
    e -> Interceptor.error(ctx, e)
  catch
    :exit, reason -> Interceptor.error(ctx, {:exit, reason})
    x -> Interceptor.error(ctx, {:caught, x})
  end

  @doc false
  @spec default(ctx()) :: ctx()
  def default(ctx), do: ctx

  @doc false
  @spec default_error(ctx(), err()) :: ctx()
  def default_error(ctx, err), do: Interceptor.error(ctx, err)

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Interceptor.Stage

      @impl Interceptor.Stage
      def enter(ctx), do: ctx

      @impl Interceptor.Stage
      def leave(ctx), do: ctx

      @impl Interceptor.Stage
      def error(ctx, err),
        do: Interceptor.error(ctx, err)

      @doc """
      """
      @spec enqueue(Interceptor.Stage.ctx(), Interceptor.Stage.name()) :: Interceptor.Stage.ctx()
      def enqueue(ctx, name \\ __MODULE__) do
        Interceptor.enqueue(ctx, [
          Interceptor.Stage.create(name, module: __MODULE__)
        ])
      end

      defoverridable enter: 1, leave: 1, error: 2
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    @impl Inspect
    def inspect(%@for{name: name, module: nil}, opts) do
      container_doc("Interceptor.Stage.create(", [name], ")", opts, &@protocol.inspect/2,
        break: :flex
      )
    end

    def inspect(%@for{name: name, module: mod}, opts) do
      container_doc(
        "Interceptor.Stage.create(",
        [name, [module: mod]],
        ")",
        opts,
        &@protocol.inspect/2,
        separator: ",",
        break: :flex
      )
    end
  end
end
