defmodule Fix do
  @moduledoc """
  Implementation of the Saga Pattern in Elixir, using middleware.

  It's a way of composing side-effectful functions with their
  compensation (i.e. a function that will "undo" the original effects).

  To visualize it, let's imagine we have a 4-step transaction. Successful
  execution flow would look like:

  ```
  [T1] -> [T2] -> [T3] -> [T4]
  ```

  and if we get a failure on 3-d step, Sage would cleanup side effects by
  running compensation functions:

  ```
  [T1] -> [T2] -> [T3 has an error]
                  ↓
  [C1] <- [C2] <- [C3]
  ```

  For more details, see [here](https://microservices.io/patterns/data/saga.html),
  [here](https://www.baeldung.com/cs/saga-pattern-microservices), and
  [here](https://www.cs.cornell.edu/andru/cs711/2002fa/reading/sagas.pdf).

  Also see the library [sage](https://github.com/Nebo15/sage).

  ## Examples

  Here's a successful example:

    iex> 42
    ...> |> start()
    ...> |> stage(fn x -> {:ok, x + 1} end, &id/2)
    ...> |> stage(fn x -> {:ok, x * 10} end, &id/2)
    ...> |> stage(fn x -> {:ok, x - 1} end, &id/2)
    ...> |> stage(fn x -> {:ok, x / 10} end, &id/2)
    ...> |> unwrap()
    {:ok, 42.9}

  And an unsuccessful example:

    iex> 42
    ...> |> start()
    ...> |> stage(fn x -> {:ok, x + 1} end, fn r, x -> [x | r] end)
    ...> |> stage(fn x -> {:ok, x * 10} end, fn r, x -> [x | r] end)
    ...> |> stage(fn x -> {:error, [x]} end, fn r, x -> [x | r] end)
    ...> |> stage(fn x -> {:ok, x / 10} end, fn r, x -> [x | r] end)
    ...> |> unwrap()
    {:error, [42, 43, nil, 430]}
  """
  use Boundary, top_level?: true, deps: [Pend, Rails], exports: [Pend]

  @type two_track(a, b) :: Rails.two_track(a, b)
  @type fix(r, a, r2) :: (r, a -> r2)
  @type fix :: fix(any, any, any)
  @type result(a, b) :: {:__fix__, two_track(a, b), [{term(), fix()}]}
  @type result :: result(any, any)

  @doc """
  Take some value and return it again.

  ## Examples

    iex> id(42)
    42
  """
  @spec id(x) :: x when x: term()
  def id(x), do: x

  @doc """
  Similar to `id/1`, but with 2 arguments.

  ## Examples

    iex> id(42, 24)
    42
  """
  @spec id(reason, x) :: reason when reason: term(), x: term()
  def id(reason, _x), do: reason

  @doc """
  Initializes the state required for compensation.
  """
  @spec start(term()) :: result()
  def start({:__fix__, x, cs}), do: x |> Rails.wrap() |> wrap(cs)
  def start(x), do: x |> Rails.wrap() |> wrap()

  @doc """
  Wraps a arbitrary value `x` as a compensatable value.
  """
  @spec wrap(term(), [{term(), fix()}]) :: result()
  def wrap(x, cs \\ []), do: {:__fix__, x, cs}

  @doc """
  Unwraps a compensatable value.
  """
  @spec unwrap(result() | term()) :: term()
  def unwrap({:__fix__, x, _cs}), do: x
  def unwrap(x), do: x

  @doc """
  Creates a compensatable function (or stage) from a `t:Rails.switch_fun/2`
  `f` (i.e. the normal action) and a `t:fix/3` `g` (i.e. the compensation).
  """
  @spec stage(Rails.switch_fun(a, c), fix(c, a, x)) :: (result(a, b) -> result(c, x))
        when a: any, b: any, c: any, x: any
  def stage(f, g), do: &stage(&1, f, g)

  @doc """
  Similar to `stage/2`, but easier to use with pipes.
  """
  @spec stage(result(a, b), Rails.switch_fun(a, c), fix(c, a, x)) :: result(c, x)
        when a: any, b: any, c: any, x: any
  def stage({:__fix__, {:ok, x}, cs}, f, g) do
    case f.(x) do
      {:ok, y} ->
        wrap({:ok, y}, [{x, g} | cs])

      {:error, reason} ->
        [{nil, g} | cs]
        |> Enum.reduce(reason, fn {x, c}, r -> c.(r, x) end)
        |> then(&{:error, &1})
        |> wrap()
    end
  end

  def stage({:__fix__, {:error, _}, _} = x, _f, _g) do
    x
  end

  def stage({:__fix__, x, cs}, _f, _g) do
    wrap({:error, {:unsupported_result, x}}, cs)
  end

  def stage(x, f, g) do
    stage(start(x), f, g)
  end
end

defmodule Fix.Pend do
  @moduledoc false

  # coveralls-ignore-start
  @doc false
  @spec bind_start(Pend.pause(s, a | Fix.two_track(a, b))) :: Pend.pause(s, Fix.result(a, b))
        when s: any, a: any, b: any
  def bind_start(m),
    do: Pend.bind(m, fn a -> Pend.return(Fix.start(a)) end)

  @doc false
  @spec bind_unwrap(Pend.pause(s, Fix.result(a, b))) :: Pend.pause(s, Fix.two_track(a, b))
        when s: any, a: any, b: any
  def bind_unwrap(m),
    do: Pend.bind(m, fn a -> Pend.return(Fix.unwrap(a)) end)

  @doc false
  @spec bind(Pend.pause(s, Fix.result(a, b)), (a -> Fix.two_track(c, d)), (d, a -> e)) ::
          Pend.pause(s, Fix.result(c, e))
        when s: any, a: any, b: any, c: any, d: any, e: any
  def bind(m, f, g),
    do: Pend.bind(m, &pstage(&1, f, g))

  @spec pstage(Fix.result(a, b), (a -> Fix.two_track(c, d)), (d, a -> e)) ::
          Pend.pause(s, Fix.result(c, e))
        when s: any, a: any, b: any, c: any, d: any, e: any
  defp pstage({:__fix__, {:ok, x}, cs}, f, g) do
    case f.(x) do
      {:ok, _} = result ->
        result
        |> Fix.wrap([{x, g} | cs])
        |> Pend.yield()

      {:error, _} = error ->
        error
        |> Fix.wrap([{x, g} | cs])
        |> compensate()
    end
  end

  defp pstage({:__fix__, {:error, _}, _cs} = x, _f, _g) do
    Pend.return(x)
  end

  defp pstage({:__fix__, x, cs}, _f, _g) do
    {:error, {:unsupported_result, x}}
    |> Fix.wrap(cs)
    |> Pend.yield()
  end

  defp pstage(x, f, g) do
    x |> Fix.start() |> pstage(f, g)
  end

  @spec compensate(Fix.result(a, b)) :: Pend.pause(s, Fix.result(c, e))
        when s: any, a: any, b: any, c: any, e: any
  defp compensate({:__fix__, {:error, _} = x, []}), do: Pend.return(x)

  defp compensate({:__fix__, {:error, reason}, [{x, g} | t]}) do
    result = Fix.wrap({:error, g.(reason, x)}, t)
    Pend.yield(fn s -> {compensate(result), s} end)
  end

  # coveralls-ignore-stop
end
