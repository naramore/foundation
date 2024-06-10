defmodule Class.Monad do
  alias Class.Applicative
  # class Applicative m => Monad (m :: Type -> Type) where
  @type t(_a) :: term()

  @instances [Maybe, List, Tuple, Function]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      # return a >>= k = k a
      left_identity: fn u, a, k ->
        bind(return(u, a), k) == k.(a)
      end,
      # m >>= return = m
      right_identity: fn u, m ->
        bind(m, &return(u, &1)) == m
      end,
      # m >>= (\x -> k x >>= h) = (m >>= k) >>= h
      associativity: fn m, k, h ->
        bind(m, fn x -> bind(k.(x), h) end) == bind(bind(m, k), h)
      end
    }
  end

  # (>>=) :: m a -> (a -> m b) -> m b
  @callback bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()

  @callback bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
  def bind(m, k), do: Class.dispatch!(__MODULE__, :bind, [m, k])

  # (>>) :: m a -> m b -> m b
  # m >> k = m >>= \_ -> k
  @spec compose(t(a), t(b)) :: t(b) when a: any(), b: any()
  def compose(m, k), do: bind(m, fn _ -> k end)

  # return :: a -> m a
  # return = pure
  defdelegate return(ta, a), to: Applicative, as: :pure

  defmacro __using__(_opts) do
    quote do
      alias Class.Monad
      @behaviour Monad
    end
  end
end

defmodule Class.Monad.List do
  use Class.Monad

  # xs >>= f = [y | x <- xs, y <- f x]
  @impl Monad
  def bind(m, k) do
    for x <- m, y <- k.(x), do: y
  end
end

defmodule Class.Monad.Tuple do
  use Class.Monad
  alias Class.Semigroup

  @impl Monad
  def bind({a, b}, k) do
    {c, d} = k.(b)
    {Semigroup.append(a, c), d}
  end
end

defmodule Class.Monad.Function do
  use Class.Monad

  # f >>= k = \ r -> k (f r) r
  @impl Monad
  def bind(m, k) do
    &k.(m.(&1)).(&1)
  end
end
