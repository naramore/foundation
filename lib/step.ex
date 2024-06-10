defmodule Step do
  @moduledoc """
  Implementation of [diet](https://github.com/pragdave/diet), with adapters
  for the pause monad in `Pend`.

  INCOMPLETE: still need to do the following
    - create the stepper struct
    - integrate with pause monad state
    - capture the return at each yield
    - support plugins
      - record history
      - track "cursor" (i.e. step + run #)
  """
  use Boundary, top_level?: true, deps: [Pend], exports: [Reductions]

  @type pause(s, a) :: Pend.pause(s, a)
  @type trigger :: term()
  @type model :: term()

  defdelegate start(a), to: Pend, as: :return
  defdelegate return(a), to: Pend

  @doc """
  `Pend.bind/2` with a `Pend.yield/0` bound afterwards.

  ## Examples

    iex> {{:suspend, m}, [] = s} = start(42)
    ...> |> bind(fn a -> return(a + 1) end)
    ...> |> step([])
    iex> step(m, s)
    {{:done, 43}, []}
  """
  @spec bind(pause(s, a), (a -> pause(s, b))) :: pause(s, b) when s: any, a: any, b: any
  def bind(m, k), do: m |> Pend.bind(k) |> Pend.bind(Pend.yield())

  @doc """
  `bind/2` for normal functions.

  ## Examples

    iex> {{:suspend, m}, [] = s} = start(42)
    ...> |> map(&(&1 + 1))
    ...> |> step([])
    iex> step(m, s)
    {{:done, 43}, []}
  """
  @spec map(pause(s, a), (a -> b)) :: pause(s, b) when s: any, a: any, b: any
  def map(m, f), do: Pend.bind(m, &Pend.yield(f.(&1)))

  defdelegate step(m, s), to: Pend
  defdelegate run(m, s), to: Pend
end
