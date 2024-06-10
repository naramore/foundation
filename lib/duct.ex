defmodule Duct do
  @moduledoc """
  https://clojure.org/reference/transducers
  """
  use Boundary, top_level?: true, deps: [Arrow], exports: []

  @typedoc """
  """
  @type t(x, acc) :: ({:complete, acc} | {:step, x, acc} -> maybe_cont_or_halt(acc))
  @type t() :: t(any(), any())

  @type reducer(x, acc) :: (x, acc -> acc)
  @type reducer_while(x, acc) :: (x, acc -> cont_or_halt(acc))
  @type educer(x, acc) :: reducer(x, acc) | reducer_while(x, acc) | t(x, acc)
  @type transducer(x, r, y, s) :: (t(x, r) -> t(y, s))
  @type transducer() :: transducer(any(), any(), any(), any())

  @type cont(x) :: {:cont, x}
  @type halt(x) :: {:halt, x}
  @type cont_or_halt(x) :: cont(x) | halt(x)
  @type maybe_cont_or_halt(x) :: cont_or_halt(x) | x

  defdelegate comp(f, g), to: Arrow, as: :compose

  @doc false
  @spec ident(x) :: x when x: any()
  def ident(x), do: x

  @doc """
  """
  @spec completing(educer(x, acc), (acc -> acc) | nil) :: t(x, acc) when x: any(), acc: any()
  def completing(f, cf \\ nil)
  def completing(f, _cf) when is_function(f, 1), do: f
  def completing(f, nil), do: completing(f, &ident/1)

  def completing(f, cf) do
    fn
      {:complete, acc} -> cf.(acc)
      {:step, x, acc} -> f.(x, acc)
    end
  end

  @doc """
  """
  @spec transduce(Enumerable.t(), transducer(x, r, y, s), educer(x, r)) :: s
        when x: any(), y: any(), r: any(), s: any()
  def transduce([acc | enum], xf, f) do
    transduce(enum, acc, xf, f)
  end

  @doc """
  """
  @spec transduce(Enumerable.t(), r, transducer(x, r, y, s), educer(x, r)) :: s
        when x: any(), y: any(), r: any(), s: any()
  def transduce(enum, acc, xf, f)

  def transduce(enum, acc, xf, f) when is_function(f, 2) do
    transduce(enum, acc, xf, completing(f))
  end

  def transduce(enum, acc, xf, f) when is_function(f, 1) do
    f = xf.(f)

    enum
    |> Enum.reduce_while(acc, fn x, acc ->
      case f.({:step, x, acc}) do
        {:cont, acc} -> {:cont, acc}
        {:halt, acc} -> {:halt, acc}
        acc -> {:cont, acc}
      end
    end)
    |> then(&f.({:complete, &1}))
  end

  @doc """
  """
  @spec into(Enumerable.t(), Collectable.t(), transducer()) :: Collectable.t()
  def into(enum, coll \\ [], xf) do
    enum
    |> transduce([], xf, &[&1 | &2])
    |> :lists.reverse()
    |> Enum.into(coll)
  end

  # @spec eduction(Enumerable.t(), transducer(x, r, s)) :: Enumerable.t() when x: any(), r: any(), s: any()
  # def eduction(enum, xf) do
  #   # Enum.reduce_while(eduction(xs, xf), acc, completing(f)) == transduce(xs, acc, xf, f)
  # end

  @doc false
  @spec unwrap(maybe_cont_or_halt(acc), (acc -> acc), (acc -> acc)) :: acc when acc: any()
  def unwrap(acc, on_cont, on_halt \\ &halt/1)
  def unwrap({:halt, acc}, _on_cont, on_halt), do: wrap(on_halt.(acc))
  def unwrap({:cont, acc}, on_cont, _on_halt), do: wrap(on_cont.(acc))
  def unwrap(acc, on_cont, on_halt), do: unwrap(cont(acc), on_cont, on_halt)

  @doc false
  @spec wrap(maybe_cont_or_halt(acc)) :: cont_or_halt(acc) when acc: any()
  def wrap({:cont, _} = acc), do: acc
  def wrap({:halt, _} = acc), do: acc
  def wrap(acc), do: cont(acc)

  @doc false
  @spec cont(acc) :: cont(acc) when acc: any()
  def cont({:cont, _} = acc), do: acc
  def cont(acc), do: {:cont, acc}

  @doc false
  @spec halt(acc) :: halt(acc) when acc: any()
  def halt({:halt, _} = acc), do: acc
  def halt(acc), do: {:halt, acc}

  @doc false
  @spec step(t(x, r), x, r) :: cont_or_halt(r) when x: any(), r: any()
  def step(xf, x, acc), do: xf.({:step, x, acc})

  @doc false
  @spec complete(t(x, r), r) :: r when x: any(), r: any()
  def complete(xf, acc), do: xf.({:complete, acc})

  @doc """
  """
  @spec map(transducer(w, r, x, s), (x -> y)) :: transducer(x, r, y, s)
        when w: any(), x: any(), y: any(), r: any(), s: any()
  def map(xf, f), do: comp(map(f), xf)

  @doc """
  """
  @spec map((x -> y)) :: transducer(x, r, y, s) when x: any(), y: any(), r: any(), s: any()
  def map(f) do
    fn rf ->
      fn
        {:complete, acc} -> complete(rf, acc)
        {:step, x, acc} -> unwrap(acc, &step(rf, f.(x), &1))
      end
    end
  end

  @doc """
  """
  @spec filter(transducer(w, r, x, s), (x -> as_boolean(any()))) :: transducer(x, r, x, s)
        when w: any(), x: any(), r: any(), s: any()
  def filter(xf, pred), do: comp(filter(pred), xf)

  @doc """
  """
  @spec filter((x -> as_boolean(any()))) :: transducer(x, r, x, s)
        when x: any(), r: any(), s: any()
  def filter(pred) do
    fn rf ->
      fn
        {:complete, acc} ->
          complete(rf, acc)

        {:step, x, acc} ->
          unwrap(acc, fn a ->
            if pred.(x) do
              step(rf, x, a)
            else
              a
            end
          end)
      end
    end
  end

  @doc """
  """
  @spec complement((x -> as_boolean(any()))) :: (x -> boolean()) when x: any()
  def complement(f) do
    fn x ->
      !f.(x)
    end
  end

  @doc """
  """
  @spec reject(transducer(w, r, x, s), (x -> as_boolean(any()))) :: transducer(x, r, x, s)
        when w: any(), x: any(), r: any(), s: any()
  def reject(xf, pred), do: comp(reject(pred), xf)

  @doc """
  """
  @spec reject((x -> as_boolean(any()))) :: transducer(x, r, x, s)
        when x: any(), r: any(), s: any()
  def reject(pred), do: filter(complement(pred))

  # cat mapcat take take-while take-nth drop drop-while replace partition-by
  # partition-all keep keep-indexed map-indexed distinct interpose dedupe random-sample

  # TODO: add a bunch more of this stuff...

  @doc """
  """
  @spec halt_when(transducer(w, r, x, s), (x -> as_boolean(any())), (x, r -> s) | nil) ::
          transducer(x, r, x, s)
        when w: any(), x: any(), r: any(), s: any()
  def halt_when(xf, pred, retf), do: comp(halt_when(pred, retf), xf)

  @doc """
  """
  @spec halt_when((x -> as_boolean(any())), (x, r -> s) | nil) :: transducer(x, r, x, s)
        when x: any(), r: any(), s: any()
  def halt_when(pred, retf \\ nil) do
    fn rf ->
      fn
        {:complete, {:__halt__, acc}} ->
          complete(rf, acc)

        {:complete, acc} ->
          complete(rf, acc)

        {:step, x, acc} ->
          unwrap(acc, fn a ->
            if pred.(x) do
              if not is_nil(retf) do
                retf.(x, complete(rf, acc))
              else
                x
              end
              |> then(&halt({:__halt__, &1}))
            else
              step(rf, x, a)
            end
          end)
      end
    end
  end
end
