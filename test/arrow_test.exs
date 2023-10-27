defmodule ArrowDocTest do
  use ExUnit.Case, async: true
  doctest Arrow, import: true
end

defmodule ArrowTest do
  @moduledoc """
  Arrow and Category laws from Haskell:
  ```haskell
  right identity        :: f . id = f
  left identity         :: id . f = f
  associativity         :: f . (g . h) = (f . g) . h
  identity              :: arr id = id
  composition           :: arr (f >>> g) = arr f >>> arr g
  first commutativity   :: first (arr f) = arr (first f)
  first composition     :: first (f >>> g) = first f >>> first g
  first decomposition   :: first f >>> arr fst = arr fst >>> f
  product commutativity :: first f >>> arr (id *** g) = arr (id *** g) >>> first f
  first reassociation   :: first (first f) >>> arr assoc = arr assoc >>> first f
  where
    assoc ((a,b),c) = (a,(b,c))
  ```

  Elixir implementations:
  ```elixir
  # right identity        :: compose(f, &id/1).(x) == f.(x)
  # left identity         :: compose(&id/1, f).(x) == f.(x)
  # associativity         :: compose(f, compose(g, h)).(x) == compose(compose(f, g), h).(x)
  # identity              :: N/A
  # composition           :: N/A
  # first commutativity   :: N/A
  # first composition     :: first(compose(f, g)).(x) == compose(first(f), first(g)).(x)
  # first decomposition   :: compose(first(f), &fst/1).(x) == compose(&fst/1, f).(x)
  # product commutativity :: compose(first(f), product(&id/1, g)).(x) == compose(product(&id/1, g), first(f)).(x)
  # first reassociation   :: compose(first(first(f)), &reassociate/1).(x) == compose(&reassociate/1, first(f)).(x)
  ```

  ArrowChoice laws from Haskell:
  ```haskell
  composition           :: left (f >>> g) = left f >>> left g
  commutativity         :: f >>> arr Left = arr Left >>> left f
  split commutativity   :: left f >>> arr (id +++ g) = arr (id +++ g) >>> left f
  left association      :: left (left f) >>> arr assocsum = arr assocsum >>> left f
  where
    assocsum (Left (Left x)) = Left x
    assocsum (Left (Right y)) = Right (Left y)
    assocsum (Right z) = Right (Right z)
  ```

  Elixir implementations:
  ```elixir
  # composition           :: left(compose(f, g)).(x) == compose(left(f), left(g)).(x)
  # commutativity         :: compose(f, &{:left, &1}).(x) == compose(&{:left, &1}, left(f)).(x)
  # split commutativity   :: compose(left(f), csplit(&id/1, g)).(x) == compose(csplit(&id/1, g), left(f)).(x)
  # left association      :: compose(left(left(f)), assoc_sum()).(x) == compose(assoc_sum(), left(f)).(x)

  def assoc_sum do
    fn
      {:left, {:left, x}} -> {:left, x}
      {:left, {:right, x}} -> {:right, {:left, x}}
      {:right, x} -> {:right, {:right, x}}
    end
  end
  ```
  """
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Arrow

  describe "Arrow Laws =>" do
    @describetag :laws

    property "right identity" do
      check all(
              f <- f(),
              x <- term()
            ) do
        assert compose(f, &id/1).(x) == f.(x)
      end
    end

    property "left identity" do
      check all(
              f <- f(),
              x <- term()
            ) do
        assert compose(&id/1, f).(x) == f.(x)
      end
    end

    property "associativity" do
      check all(
              {f, g, h} <- tuple({f(), f(), f()}),
              x <- term()
            ) do
        assert compose(f, compose(g, h)).(x) == compose(compose(f, g), h).(x)
      end
    end

    property "first composition" do
      check all(
              {f, g} <- tuple({f(), f()}),
              x <- tuple({term(), term()})
            ) do
        assert first(compose(f, g)).(x) == compose(first(f), first(g)).(x)
      end
    end

    property "first decomposition" do
      check all(
              f <- f(),
              x <- tuple({term(), term()})
            ) do
        assert compose(first(f), &fst/1).(x) == compose(&fst/1, f).(x)
      end
    end

    property "product commutativity" do
      check all(
              {f, g} <- tuple({f(), f()}),
              x <- tuple({term(), term()})
            ) do
        assert compose(first(f), product(&id/1, g)).(x) ==
                 compose(product(&id/1, g), first(f)).(x)
      end
    end

    property "first reassociation" do
      check all(
              f <- f(),
              x <- tuple({tuple({term(), term()}), term()})
            ) do
        assert compose(first(first(f)), &reassociate/1).(x) ==
                 compose(&reassociate/1, first(f)).(x)
      end
    end
  end

  describe "ArrowChoice Laws =>" do
    @describetag :laws

    property "composition" do
      check all(
              {f, g} <- tuple({f(), f()}),
              x <- gen_either()
            ) do
        assert left(compose(f, g)).(x) == compose(left(f), left(g)).(x)
      end
    end

    property "commutativity" do
      check all(
              f <- f(),
              x <- term()
            ) do
        assert compose(f, &{:left, &1}).(x) == compose(&{:left, &1}, left(f)).(x)
      end
    end

    property "split commutativity" do
      check all(
              {f, g} <- tuple({f(), f()}),
              x <- gen_either()
            ) do
        assert compose(left(f), csplit(&id/1, g)).(x) == compose(csplit(&id/1, g), left(f)).(x)
      end
    end

    property "left association" do
      check all(
              f <- f(),
              x <- gen_either(gen_either())
            ) do
        assert compose(left(left(f)), &assoc_sum/1).(x) == compose(&assoc_sum/1, left(f)).(x)
      end
    end
  end

  describe "Arrow" do
    property "id/1 returns what is passed" do
      check all(x <- term()) do
        assert x == Arrow.id(x)
      end
    end

    property "compose/2 equals |>/2" do
      check all(x <- integer()) do
        f = fn x -> x + 10 end
        g = fn x -> x * 3 end
        assert Arrow.compose(f, g).(x) == x |> f.() |> g.()
      end
    end

    property "first/1 transforms 1st element" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              {x, y} <- tuple({term(), term()})
            ) do
        assert {xx, _} = Arrow.first(f).({x, y})
        assert f.(x) == xx
      end
    end

    property "first/1 id's 2nd element" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              {x, y} <- tuple({term(), term()})
            ) do
        assert {_, ^y} = Arrow.first(f).({x, y})
      end
    end

    property "second/1 id's 1st element" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              {x, y} <- tuple({term(), term()})
            ) do
        assert {^x, _} = Arrow.second(f).({x, y})
      end
    end

    property "second/1 transforms 2nd element" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              {x, y} <- tuple({term(), term()})
            ) do
        assert {_, yy} = Arrow.second(f).({x, y})
        assert f.(y) == yy
      end
    end

    property "product/2 transforms both element" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              g <- map(term(), fn m -> fn _ -> m end end),
              {x, y} <- tuple({term(), term()})
            ) do
        assert {xx, yy} = Arrow.product(f, g).({x, y})
        assert f.(x) == xx
        assert g.(y) == yy
      end
    end

    property "split/1 copies into 2-tuple" do
      check all(x <- term()) do
        assert {^x, ^x} = Arrow.split(x)
      end
    end

    property "unsplit/2 combines 2-tuple" do
      check all({x, y} <- tuple({term(), term()})) do
        assert [^x, ^y] = Arrow.unsplit({x, y}, fn a, b -> [a, b] end)
      end
    end

    property "unsplit/2 undoes split/1" do
      check all(x <- term()) do
        assert x == x |> Arrow.split() |> Arrow.unsplit(fn x, _ -> x end)
      end
    end

    property "fanout/2 equals split/1 |> product/2" do
      check all(
              f <- map(term(), fn m -> fn _ -> m end end),
              g <- map(term(), fn m -> fn _ -> m end end),
              x <- term()
            ) do
        assert Arrow.fanout(f, g).(x) == x |> Arrow.split() |> Arrow.product(f, g).()
      end
    end

    property "swap/1 swaps elements" do
      check all({x, y} <- tuple({term(), term()})) do
        assert {^y, ^x} = Arrow.swap({x, y})
      end
    end

    property "reassociate/2 shuffles nested tuples" do
      check all(
              a <-
                one_of([
                  tuple({tuple({term(), term()}), term()}),
                  tuple({term(), tuple({term(), term()})})
                ])
            ) do
        case a do
          {{x, y}, z} ->
            assert {^x, {^y, ^z}} = Arrow.reassociate(a)

          {x, {y, z}} ->
            assert {{^x, ^y}, ^z} = Arrow.reassociate(a)
        end
      end
    end
  end

  describe "Arrow Choice" do
    property "left/1 transforms left" do
      check all(
              y <- term(),
              f = fn _ -> y end,
              x <- term()
            ) do
        assert {:left, ^y} = Arrow.left(f).({:left, x})
      end
    end

    property "left/1 id's right" do
      check all(
              y <- term(),
              f = fn _ -> y end,
              x <- term()
            ) do
        assert {:right, ^x} = Arrow.left(f).({:right, x})
      end
    end

    property "right/1 id's left" do
      check all(
              y <- term(),
              f = fn _ -> y end,
              x <- term()
            ) do
        assert {:left, ^x} = Arrow.right(f).({:left, x})
      end
    end

    property "right/1 transforms right" do
      check all(
              y <- term(),
              f = fn _ -> y end,
              x <- term()
            ) do
        assert {:right, ^y} = Arrow.right(f).({:right, x})
      end
    end

    property "csplit/1 transforms left w/ f" do
      check all(
              {y, z} <- tuple({term(), term()}),
              f = fn _ -> y end,
              g = fn _ -> z end,
              x <- term()
            ) do
        assert {:left, ^y} = Arrow.csplit(f, g).({:left, x})
      end
    end

    property "csplit/1 transforms right w/ g" do
      check all(
              {y, z} <- tuple({term(), term()}),
              f = fn _ -> y end,
              g = fn _ -> z end,
              x <- term()
            ) do
        assert {:right, ^z} = Arrow.csplit(f, g).({:right, x})
      end
    end

    property "fanin/2 runs f given left" do
      check all(
              {y, z} <- tuple({term(), term()}),
              f = fn _ -> y end,
              g = fn _ -> z end,
              x <- term()
            ) do
        assert y == Arrow.fanin(f, g).({:left, x})
      end
    end

    property "fanin/2 runs g given right" do
      check all(
              {y, z} <- tuple({term(), term()}),
              f = fn _ -> y end,
              g = fn _ -> z end,
              x <- term()
            ) do
        assert z == Arrow.fanin(f, g).({:right, x})
      end
    end

    property "mirror/1 changes left to right" do
      check all(x <- term()) do
        assert {:right, ^x} = Arrow.mirror({:left, x})
      end
    end

    property "mirror/1 changes right to left" do
      check all(x <- term()) do
        assert {:left, ^x} = Arrow.mirror({:right, x})
      end
    end
  end

  def f(gen \\ term()) do
    map(gen, fn x -> fn _ -> x end end)
  end

  def gen_either(gen \\ term()) do
    one_of([tuple({constant(:left), gen}), tuple({constant(:right), gen})])
  end

  def assoc_sum({:left, {:left, x}}), do: {:left, x}
  def assoc_sum({:left, {:right, x}}), do: {:right, {:left, x}}
  def assoc_sum({:right, x}), do: {:right, {:right, x}}
end
