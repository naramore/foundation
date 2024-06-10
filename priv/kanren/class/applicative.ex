defmodule Class.Applicative do
  import Class, only: [comp: 2, const: 2, id: 1]
  alias Class.Functor
  # class Functor f => Applicative (f :: Type -> Type) where
  @type t(_a) :: term()

  @instances [Maybe, List, Tuple, Function]

  @spec instances() :: [module()]
  def instances, do: @instances

  def laws do
    %{
      # pure id <*> v = v
      identity: fn v ->
        convey(v, pure(v, &id/1)) == v
      end,
      # pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
      composition: fn u, v, w ->
        compose = pure(u, fn a -> fn b -> comp(a, b) end end)
        convey(w, convey(v, convey(u, compose))) == convey(convey(w, v), u)
      end,
      # pure f <*> pure x = pure (f x)
      homomorphism: fn u, f, x ->
        convey(pure(u, x), pure(u, f)) == pure(u, f.(x))
      end,
      # u <*> pure y = pure ($ y) <*> u
      # ($) :: (a -> b) -> a -> b
      interchange: fn u, y ->
        convey(pure(u, y), u) == convey(u, pure(u, &(&1.(y))))
      end
    }
  end

  # pure :: a -> f a
  @callback pure(t(a), a) :: t(a) when a: any()

  # (<*>) :: f (a -> b) -> f a -> f b
  @callback convey(t(a), t((a -> b))) :: t(b) when a: any(), b: any()

  @spec pure(t(a), a) :: t(a) when a: any()
  def pure(ta, a), do: Class.dispatch!(__MODULE__, :pure, [ta, a])

  @spec convey(t(a), t((a -> b))) :: t(b) when a: any(), b: any()
  def convey(ta, tf), do: Class.dispatch!(__MODULE__, :convey, [ta, tf])

  # liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  # liftA2 f x = (<*>) (fmap f x)
  @spec lift(t(a), t(b), (a, b -> c)) :: t(c) when a: any(), b: any(), c: any()
  def lift(ta, tb, f) do
    convey(tb, Functor.fmap(ta, fn a -> &f.(a, &1) end))
  end

  # (*>) :: f a -> f b -> f b
  # a1 *> a2 = (id <$ a1) <*> a2
  @spec then(t(a), t(b)) :: t(b) when a: any(), b: any()
  def then(ta, tb), do: convey(tb, following(&id/1, ta))

  # (<*) :: f a -> f b -> f a
  # (<*) = liftA2 const
  @spec following(t(a), t(b)) :: t(a) when a: any(), b: any()
  def following(ta, tb), do: lift(ta, tb, &const/2)

  defmacro __using__(_opts) do
    quote do
      alias Class.Applicative
      @behaviour Applicative
    end
  end
end

defmodule Class.Applicative.List do
  use Class.Applicative

  # pure x    = [x]
  @impl Applicative
  def pure(_ta, a), do: [a]

  # fs <*> xs = [f x | f <- fs, x <- xs]
  @impl Applicative
  def convey(ta, tf) do
    for f <- tf, a <- ta, do: f.(a)
  end
end

defmodule Class.Applicative.Tuple do
  use Class.Applicative
  alias Class.Monoid

  @impl Applicative
  def pure(ta, a) do
    size = tuple_size(ta)

    ta
    |> elem(0)
    |> Monoid.mempty()
    |> Tuple.duplicate(size)
    |> put_elem(size - 1, a)
  end

  @impl Applicative
  def convey({v, w}, {a, fun}), do: {v <> a, fun.(w)}
  def convey({v, w, x}, {a, b, fun}), do: {v <> a, w <> b, fun.(x)}
  def convey({v, w, x, y}, {a, b, c, fun}), do: {v <> a, w <> b, x <> c, fun.(y)}

  def convey({v, w, x, y, z}, {a, b, c, d, fun}) do
    {
      a <> v,
      b <> w,
      c <> x,
      d <> y,
      fun.(z)
    }
  end

  def convey(tuple_a, tuple_b) when tuple_size(tuple_a) == tuple_size(tuple_b) do
    last_index = tuple_size(tuple_a) - 1

    tuple_a
    |> Tuple.to_list()
    |> Enum.zip(Tuple.to_list(tuple_b))
    |> Enum.with_index()
    |> Enum.map(fn
      {{arg, fun}, ^last_index} -> fun.(arg)
      {{left, right}, _} -> left <> right
    end)
    |> List.to_tuple()
  end
end

defmodule Class.Applicative.Function do
  use Class.Applicative

  # pure = const
  @impl Applicative
  def pure(_ta, a), do: &Class.const(a, &1)

  # (<*>) f g x = f x (g x)
  @impl Applicative
  def convey(ta, tf) do
    fn x -> tf.(x).(ta.(x)) end
  end
end
