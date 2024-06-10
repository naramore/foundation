defmodule Arrow do
  @moduledoc """
  Partial implementation of Arrows in Elixir.

  Arrows let you think diagrammatically, and is a powerful way of thinking
  about flow programming, concurrency, and more.

                   ┌---> f --------------------------┐
                   |                                 v
      input ---> split                            unsplit ---> result
                   |                                 ^
                   |              ┌--- h ---┐        |
                   |              |         v        |
                   └---> g ---> split     unsplit ---┘
                                  |         ^
                                  └--- i ---┘

  For more details, see
  [Haskell/Understanding Arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows),
  and [Arrows: A General Interface to Computation](https://www.haskell.org/arrows/).

  ## Examples

  This example is built from [this model](https://youtu.be/DYtl42sjaZk?si=00xp4gsZ7YzHkydp&t=1302):

    iex> 9
    ...> |> pipe(fanout(
    ...>   fn x -> x / 5 end,
    ...>   fn y -> y + 1 end
    ...>   |> compose(fanout(
    ...>     &inspect/1,
    ...>     &id/1
    ...>   ))
    ...>   |> compose(unsplit(
    ...>     fn a, b -> inspect({b, a}) end
    ...>   ))
    ...> ))
    ...> |> pipe(unsplit(
    ...>   &String.at(&2, round(&1))
    ...> ))
    "0"

  ## Notes

  [witchcraft](https://github.com/witchcrafters/witchcraft) was a huge
  inspiration for this module and much of the documentation was taken
  from it.
  """
  use Boundary, top_level?: true, deps: [], exports: []

  @typedoc """
  A 1-arity function.
  """
  @type t(a, b) :: (a -> b)

  @typedoc """
  Represents a value with two possibilities: left, and right.

  For more details see Haskell's documentation on
  [Data.Either](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html).
  """
  @type either(a, b) :: {:left, a} | {:right, b}

  @doc """
  Take some value and return it again.

  ## Examples

    iex> id(42)
    42
  """
  @spec id(t(a, b)) :: t(a, b) when a: any, b: any
  def id(x), do: x

  @doc """
  Returns the first element of a 2-tuple.

  ## Examples

    iex> fst({1, 2})
    1
  """
  @spec fst({a, b}) :: a when a: any, b: any
  def fst({a, _}), do: a

  @doc """
  Take two morphisms and return their composition "the math way".
  That is, `(b -> c) -> (a -> b) -> (a -> c)`.

  ## Examples

    iex> times_ten_plus_one = compose(fn x -> x * 10 end, fn y -> y + 1 end)
    ...> times_ten_plus_one.(5)
    51
  """
  @spec compose(t(a, b), t(b, c)) :: t(a, c)
        when a: any, b: any, c: any
  def compose(f, g), do: fn x -> x |> f.() |> g.() end

  @doc """
  Pipe some data through a morphism.

  Similar to `apply/2`, but with a single argument, not needing to wrap
  the argument in a list.

  ## Examples

    iex> pipe(42, &(&1 + 1))
    43
  """
  @spec pipe(a, t(a, b)) :: b when a: any, b: any
  # credo:disable-for-next-line Credo.Check.Refactor.Apply
  def pipe(x, f), do: apply(f, [x])

  @doc """
  Take two arguments (as a 2-tuple), and run one function on the left side (first element),
  and run a different function on the right side (second element).

        ┌------> f.(a) = x -------┐
        |                         v
      {a, b}                    {x, y}
        |                         ^
        └------> g.(b) = y -------┘

  ## Examples

      iex> product(&(&1 - 10), &(&1 <> "!")).({42, "Hi"})
      {32, "Hi!"}
  """
  @spec product(t(a, c), t(b, d)) :: t({a, b}, {c, d})
        when a: any, b: any, c: any, d: any
  def product(f, g), do: compose(first(f), second(g))

  @doc """
  Target the first element of a tuple.

  ## Examples

    iex> first(fn x -> x * 50 end).({1, 1})
    {50, 1}
  """
  @spec first(t(a, b)) :: t({a, x}, {b, x})
        when a: any, b: any, x: any
  def first(f), do: fn {x, y} -> {f.(x), id(y)} end

  @doc """
  Target the first element of a tuple.

  ## Examples

    iex> second(fn x -> x * 50 end).({1, 1})
    {1, 50}
  """
  @spec second(t(a, b)) :: t({x, a}, {x, b})
        when a: any, b: any, x: any
  def second(f), do: fn {x, y} -> {id(x), f.(y)} end

  @doc """
  Duplicate incoming data into both halves of a 2-tuple, and run one function
  on the left copy, and a different function on the right copy.

               ┌------> f.(a) = x ------┐
               |                        v
      a ---> split = {a, a}           {x, y}
               |                        ^
               └------> g.(a) = y ------┘

  ## Examples

    iex> 42 |> pipe(fanout(&(&1 - 10), &(inspect(&1) <> "!")))
    {32, "42!"}
  """
  @spec fanout(t(a, b), t(a, c)) :: t(a, {b, c}) when a: any, b: any, c: any
  def fanout(f, g), do: compose(&split/1, product(f, g))

  @doc """
  Swap positions of elements in a tuple.

  ## Examples

    iex> swap({1, 2})
    {2, 1}
  """
  @spec swap({x, y}) :: {y, x} when x: any, y: any
  def swap({x, y}), do: {y, x}

  @doc """
  Copy a single value into both positions of a 2-tuple.

  This is useful is you want to run functions on the input separately.

  ## Examples

    iex> split(42)
    {42, 42}

    iex> 5
    ...> |> split()
    ...> |> pipe(second(fn x -> x - 2 end))
    ...> |> pipe(first(fn y -> y * 10 end))
    ...> |> pipe(second(&inspect/1))
    {50, "3"}
  """
  @spec split(x) :: {x, x} when x: any()
  def split(x), do: {x, x}

  @doc """
  Similar to `unsplit/2`, but easier to use with `compose/2`.
  """
  @spec unsplit((x, y -> z)) :: ({x, y} -> z) when x: any, y: any, z: any
  def unsplit(combine), do: &unsplit(&1, combine)

  @doc """
  Merge two tuple values with a combining function.

  ## Examples

    iex> unsplit({1, 2}, &+/2)
    3
  """
  @spec unsplit({x, y}, (x, y -> z)) :: z when x: any, y: any, z: any
  def unsplit({x, y}, combine), do: combine.(x, y)

  @doc """
  Switch the associativity of a nested tuple. Helpful since many arrows act
  on a subset of a tuple, and you may want to move portions in and out of
  that stream.

  ## Examples

    iex> reassociate({1, {2, 3}})
    {{1, 2}, 3}

    iex> reassociate({{1, 2}, 3})
    {1, {2, 3}}
  """
  @spec reassociate({x, {y, z}} | {{x, y}, z}) :: {x, {y, z}} | {{x, y}, z}
        when x: any, y: any, z: any
  def reassociate({{x, y}, z}), do: {x, {y, z}}
  def reassociate({x, {y, z}}), do: {{x, y}, z}

  @doc """
  Feed left-marked inputs (i.e. `{:left, _}`) through function `f`, the rest
  are passed through unchanged.

  ## Examples

    iex> lf = left(fn x -> x + 10 end)
    iex> lf.({:left, 3})
    {:left, 13}
    iex> lf.({:right, 3})
    {:right, 3}
  """
  @spec left(t(a, b)) :: t(either(a, c), either(b, c))
        when a: any, b: any, c: any
  def left(f), do: csplit(f, &id/1)

  @doc """
  Feed right-marked inputs (i.e. `{:right, _}`) through function `f`, the rest
  are passed through unchanged.

  The `mirror/1` of `left/1`.

  ## Examples

    iex> rf = right(fn x -> x + 10 end)
    iex> rf.({:left, 3})
    {:left, 3}
    iex> rf.({:right, 3})
    {:right, 13}
  """
  @spec right(t(a, b)) :: t(either(c, a), either(c, b))
        when a: any, b: any, c: any
  def right(f), do: csplit(&id/1, f)

  @doc """
  A composition of `left/1` and `right/1`.

    - given `{:left, x}` -> runs f.(x)
    - given `{:right, y}` -> runs g.(y)

  ## Examples

    iex> sf = csplit(
    ...>    fn x -> x - 10 end,
    ...>    fn x -> x + 5 end
    ...> )
    iex> sf.({:left, 42})
    {:left, 32}
    iex> sf.({:right, 42})
    {:right, 47}
  """
  @spec csplit(t(a, c), t(b, d)) :: t(either(a, b), either(c, d))
        when a: any, b: any, c: any, d: any
  def csplit(f, g), do: fanin(&{:left, f.(&1)}, &{:right, g.(&1)})

  @doc """
  Similar to `csplit/2` but returns a value, rather than an `t:either/2`.

  ## Examples

    iex> ff = fanin(
    ...>    fn x -> x - 10 end,
    ...>    fn x -> x + 5 end
    ...> )
    iex> ff.({:left, 42})
    32
    iex> ff.({:right, 42})
    47
  """
  @spec fanin(t(a, c), t(b, c)) :: t(either(a, b), c)
        when a: any, b: any, c: any
  def fanin(f, g), do: either(f, g)

  @doc """
  Returns a function that takes an `t:either/2` as input, and returns the
  result of either `f` or `g`, based on whether the input was left or right.

  ## Examples

    iex> ef = either(
    ...>    fn x -> x - 10 end,
    ...>    fn x -> x + 5 end
    ...> )
    iex> ef.({:left, 42})
    32
    iex> ef.({:right, 42})
    47
  """
  @spec either(t(a, c), t(b, c)) :: t(either(a, b), c) when a: any, b: any, c: any
  def either(f, g), do: &either(f, g, &1)

  @doc """
  Similar to `either/2`, but easier to use with pipes.
  """
  @spec either(t(a, c), t(b, c), either(a, b)) :: c when a: any, b: any, c: any
  def either(f, _, {:left, x}), do: f.(x)
  def either(_, g, {:right, x}), do: g.(x)

  @doc """
  Swaps left with right, and vice-versa.

  ## Examples

    iex> mirror({:left, 42})
    {:right, 42}

    iex> mirror({:right, :foo})
    {:left, :foo}
  """
  @spec mirror(either(a, b)) :: either(b, a) when a: any, b: any
  def mirror({:left, x}), do: {:right, x}
  def mirror({:right, x}), do: {:left, x}
end
