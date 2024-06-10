defmodule Class.Functor do
  import Class, only: [id: 1, comp: 2]
  # class Functor (f :: Type -> Type) where
  @type t(_a) :: term()

  @instances [Maybe, List, Tuple, Map, Function]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      identity: fn x ->
        fmap(x, &id/1) == id(x)
      end,
      composition: fn x, f, g ->
        fmap(x, comp(f, g)) == comp(fmap(f), fmap(g)).(x)
      end
    }
  end

  # fmap :: (a -> b) -> f a -> f b
  @callback fmap(t(a), (a -> b)) :: t(b) when a: any(), b: any()

  @spec fmap(t(a), (a -> b)) :: t(b) when a: any(), b: any()
  def fmap(a, f), do: Class.dispatch!(__MODULE__, :fmap, [a, f])

  @spec fmap((a -> b)) :: (t(a) -> t(b)) when a: any(), b: any()
  def fmap(f), do: &fmap(&1, f)

  @spec replace(t(a), b) :: t(b) when a: any(), b: any()
  def replace(a, b), do: fmap(a, &Class.const(b, &1))

  defmacro __using__(_opts) do
    quote do
      alias Class.Functor
      @behaviour Functor
    end
  end
end

defmodule Class.Functor.List do
  use Class.Functor

  @impl Functor
  def fmap(a, f), do: Enum.map(a, f)
end

defmodule Class.Functor.Tuple do
  use Class.Functor

  @impl Functor
  def fmap({}, _f), do: {}
  def fmap({a}, f), do: {f.(a)}
  def fmap(a, f) when is_tuple(a) do
    i = tuple_size(a) - 1
    a
    |> elem(i)
    |> f.()
    |> then(&put_elem(a, i, &1))
  end
end

defmodule Class.Functor.Map do
  use Class.Functor

  @impl Functor
  def fmap(%{} = a, f) do
    a
    |> Enum.to_list()
    |> Functor.fmap(fn {k, v} -> {k, f.(v)} end)
    |> Enum.into(%{})
  end
end

defmodule Class.Functor.Function do
  use Class.Functor
  import Class, only: [comp: 2]

  @impl Functor
  def fmap(a, f) when is_function(a, 1), do: comp(f, a)
  def fmap(a, f) when is_function(a, 2),
    do: comp(f, fn x -> fn y -> a.(x, y) end end)
  def fmap(a, f) when is_function(a, 3),
    do: comp(f, fn x -> fn y, z -> a.(x, y, z) end end)
  def fmap(a, f) when is_function(a, 4),
    do: comp(f, fn w -> fn x, y, z -> a.(w, x, y, z) end end)
end
