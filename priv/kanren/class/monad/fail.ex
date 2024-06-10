defmodule Class.Monad.Fail do
  alias Class.Monad
  # class Monad m => MonadFail (m :: Type -> Type) where
  @type t(_a) :: term()

  @instances [Maybe, List]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      # fail s >>= f  =  fail s
      unnamed: fn u, s, f ->
        Monad.bind(fail(u, s), f) == fail(u, s)
      end
    }
  end

  # fail :: String -> m a
  @callback fail(t(a), String.t()) :: t(a) when a: any()

  @spec fail(t(a), String.t()) :: t(a) when a: any()
  def fail(a, s), do: Class.dispatch!(__MODULE__, :fail, [a, s])

  defmacro __using__(_opts) do
    quote do
      alias Class.Monad
      @behaviour Monad.Fail
    end
  end
end

defmodule Class.Monad.Fail.List do
  use Class.Monad.Fail

  @impl Monad.Fail
  def fail(_, _), do: []
end
