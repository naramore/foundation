defmodule Class.Monad.Plus do
  alias Class.Alternative
  # class (Alternative m, Monad m) => MonadPlus (m :: Type -> Type) where
  @type t(_a) :: term()

  # https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
  def laws, do: %{}

  # mzero :: m a
  defdelegate mzero(a), to: Alternative, as: :empty

  # mplus :: m a -> m a -> m a
  defdelegate mplus(a, b), to: Alternative, as: :assoc

  defmacro __using__(_opts) do
    quote do
    end
  end
end
