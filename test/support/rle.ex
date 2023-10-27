defmodule Step.RLE do
  @moduledoc false
  use Step.Reductions

  reductions do
    {:rle, {[], result}} ->
      {:done, Enum.reverse(result)}

    {:rle, {[{a, n}, a | tail], result}} ->
      {:rle, {[{a, n + 1} | tail], result}}

    {:rle, {[a, a | tail], result}} ->
      {:rle, {[{a, 2} | tail], result}}

    {:rle, {[a | tail], result}} ->
      {:rle, {tail, [a | result]}}

    {:rle, list} ->
      {:rle, {list, []}}
  end
end
