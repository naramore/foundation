defmodule Class.Semigroup do
  # class Semigroup a where
  @type t(_a) :: term()
  @type t() :: t(any())

  @instances [Maybe, List, Tuple, Map, MapSet, Function, BitString, Integer, Float]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      # x <> (y <> z) = (x <> y) <> z
      associativity: fn x, y, z ->
        append(x, append(y, z)) == append(append(x, y), z)
      end
    }
  end

  @callback append(t(), t()) :: t()

  @spec append(t(), t()) :: t()
  def append(a, b), do: Class.dispatch!(__MODULE__, :append, [a, b])

  # @spec sconcat(t()) :: t()
  # @spec stimes(t(), pos_integer()) :: t()

  defmacro __using__(_opts) do
    quote do
      alias Class.Semigroup
      @behaviour Semigroup
    end
  end
end

defmodule Class.Semigroup.List do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: a ++ b
end

defmodule Class.Semigroup.Integer do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: a + b
end

defmodule Class.Semigroup.Float do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: a + b
end

defmodule Class.Semigroup.BitString do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: a <> b
end

defmodule Class.Semigroup.MapSet do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: MapSet.union(a, b)
end

defmodule Class.Semigroup.Map do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: Map.merge(a, b)
end

defmodule Class.Semigroup.Function do
  use Class.Semigroup

  @impl Semigroup
  def append(a, b), do: Class.comp(b, a)
end

defmodule Class.Semigroup.Tuple do
  use Class.Semigroup

  def append(a, b) do
    a
    |> Tuple.to_list()
    |> Enum.zip(Tuple.to_list(b))
    |> Enum.map(fn {x, y} -> Semigroup.append(x, y) end)
    |> List.to_tuple()
  end
end
