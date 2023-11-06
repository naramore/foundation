defmodule Interceptor.Stage do
  @moduledoc """
  """

  defstruct enter: nil,
            leave: nil,
            error: nil

  @type t :: %__MODULE__{
          enter: stage_fun,
          leave: stage_fun,
          error: error_fun
        }

  @type ctx :: Interceptor.ctx()
  @type err :: Interceptor.err()
  @type stage_fun :: (ctx() -> ctx())
  @type error_fun :: (ctx(), err() -> ctx())
  @type opt ::
          {:enter, stage_fun | nil}
          | {:leave, stage_fun | nil}
          | {:error, error_fun | nil}

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
  @spec new([opt]) :: t
  def new(opts \\ []) do
    new(
      Keyword.get(opts, :enter, &default/1),
      Keyword.get(opts, :leave, &default/1),
      Keyword.get(opts, :error, &default_error/2)
    )
  end

  @doc false
  @spec new(stage_fun | nil, stage_fun | nil, error_fun | nil) :: t
  def new(enter, leave, error) do
    %__MODULE__{
      enter: enter,
      leave: leave,
      error: error
    }
  end

  @doc """
  """
  @spec create(keyword()) :: t() | nil
  def create(opts \\ []) do
    cond do
      Keyword.has_key?(opts, :module) ->
        module = Keyword.get(opts, :module)

        new(
          enter: &module.enter/1,
          leave: &module.leave/1,
          error: &module.error/2
        )

      Keyword.has_key?(opts, :map) ->
        opts
        |> Keyword.get(:map)
        |> Enum.into([])
        |> new()

      true ->
        new(opts)
    end
  end

  @doc """
  """
  @spec invoke(t(), ctx()) :: ctx()
  def invoke(stage, ctx) do
    case ctx do
      %{__status__: :enter} -> stage.enter.(ctx)
      %{__status__: :leave} -> stage.leave.(ctx)
      %{__status__: {:error, err}} -> stage.error.(ctx, err)
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

      defoverridable enter: 1, leave: 1, error: 2
    end
  end
end
