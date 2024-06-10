defmodule Class.Monoid do
  alias Class.Semigroup
  # class Semigroup a => Monoid a where
  @type t(_a) :: term()
  @type t() :: t(any())

  @instances [Maybe, List, Tuple, Map, MapSet, Function, BitString, Integer, Float]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      # x <> mempty = x
      right_identity: fn u, x ->
        mappend(x, mempty(u)) == x
      end,
      # mempty <> x = x
      left_identity: fn u, x ->
        mappend(mempty(u), x) == x
      end,
      # NOTE: this law is unnecessary b/c of the way mconcat/1 is defined
      # mconcat = foldr (<>) mempty
      # concatenation: nil,
    }
  end

  @callback mempty(t()) :: t()

  @spec mempty(t()) :: t()
  def mempty(a), do: Class.dispatch!(__MODULE__, :mempty, [a])

  defdelegate mappend(a, b), to: Semigroup, as: :append

  @spec mconcat([t(), ...]) :: t()
  def mconcat([x | _] = xs), do: :lists.foldr(&mappend/2, mempty(x), xs)

  defmacro __using__(_opts) do
    quote do
      alias Class.Monoid
      @behaviour Monoid
    end
  end
end

defmodule Class.Monoid.List do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: []
end

defmodule Class.Monoid.Integer do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: 0
end

defmodule Class.Monoid.Float do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: 0.0
end

defmodule Class.Monoid.BitString do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: ""
end

defmodule Class.Monoid.MapSet do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: MapSet.new()
end

defmodule Class.Monoid.Map do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: %{}
end

defmodule Class.Monoid.Function do
  use Class.Monoid

  @impl Monoid
  def mempty(_), do: &Class.id/1
end

defmodule Class.Monoid.Tuple do
  use Class.Monoid
  alias Class.Functor

  def mempty(a), do: Functor.fmap(a, &Monoid.mempty/1)
end
