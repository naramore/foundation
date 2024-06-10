defmodule Class.Monad.Free do
  alias Class.Monad
  # class Monad m => MonadFree f m | m -> f where
  @type t(_a) :: term()

  @instances [Free]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws, do: %{}

  defmacro __using__(_opts) do
    quote do
      alias Class.Monad
      @behaviour Monad.Free
    end
  end
end
