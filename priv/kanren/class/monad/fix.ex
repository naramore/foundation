# defmodule Class.Monad.Fix do
#   alias Class.Monad
#   # class Monad m => MonadFail (m :: Type -> Type) where
#   @type t(_a) :: term()

#   @instances [Maybe, List]

#   @spec instances() :: [module()]
#   def instances, do: @instances

#   def laws do
#     %{
#     }
#   end

#   # mfix :: (a -> m a) -> m a
#   @callback mfix(t(a), (a -> t(a))) :: t(a) when a: any()

#   @spec mfix(t(a), (a -> t(a))) :: t(a) when a: any()
#   def mfix(a, f), do: Class.dispatch!(__MODULE__, :mfix, [a, f])

#   @spec fix((a -> a)) :: a
#   def fix(f), do:

#   defmacro __using__(_opts) do
#     quote do
#       alias Class.Monad
#       @behaviour Monad.Fix
#     end
#   end
# end

# defmodule Class.Monad.Fix.List do
#   use Class.Monad.Fix

#   # mfix f = case fix (f . head) of
#   #              []    -> []
#   #              (x:_) -> x : mfix (drop 1 . f)
#   @impl Monad.Fix
#   def fix(_, f) do

#   end
# end
