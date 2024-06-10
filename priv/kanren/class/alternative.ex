defmodule Class.Alternative do
  alias Class.Applicative
  # class Applicative f => Alternative f where
  @type t(_a) :: term()

  @instances [Maybe, List, Tuple, Function]

  @spec instances() :: [module()]
  def instances, do: @instances

  # https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
  def laws do
    %{}
  end

  # empty :: f a
  @callback empty(t(a)) :: t(a) when a: any()

  # (<|>) :: f a -> f a -> f a
  @callback assoc(t(a), t(a)) :: t(a) when a: any()

  @spec empty(t(a)) :: t(a) when a: any()
  def empty(a), do: Class.dispatch!(__MODULE__, :empty, [a])

  @spec assoc(t(a), t(a)) :: t(a) when a: any()
  def assoc(a, b), do: Class.dispatch!(__MODULE__, :assoc, [a, b])

  # many :: f a -> f [a]
  @spec many(t(a)) :: t([a]) when a: any()
  def many(v), do: assoc(some(v), Applicative.pure(v, []))

  # some :: f a -> f [a]
  @spec some(t(a)) :: t([a]) when a: any()
  def some(v), do: Applicative.lift(v, many(v), &[&1 | &2])

  defmacro __using__(_opts) do
    quote do
      alias Class.Alternative
      @behaviour Alternative
    end
  end
end

defmodule Class.Alternative.Function do
  use Class.Alternative
  alias Class.{Monoid, Semigroup}

  @impl Alternative
  def empty(a), do: Monoid.Function.mempty(a)

  @impl Alternative
  def assoc(a, b), do: Semigroup.Function.append(a, b)
end

defmodule Class.Alternative.List do
  use Class.Alternative
  alias Class.{Monoid, Semigroup}

  @impl Alternative
  def empty(a), do: Monoid.List.mempty(a)

  @impl Alternative
  def assoc(a, b), do: Semigroup.List.append(a, b)
end

defmodule Class.Alternative.Tuple do
  use Class.Alternative
  alias Class.{Monoid, Semigroup}

  @impl Alternative
  def empty(a), do: Monoid.Tuple.mempty(a)

  @impl Alternative
  def assoc(a, b), do: Semigroup.Tuple.append(a, b)
end
