defmodule Fix.Generators do
  @moduledoc false
  require Logger

  def check_forward_flow(result, n, ep)
  def check_forward_flow([], _n, nil), do: true
  def check_forward_flow([], _n, _ep), do: false

  def check_forward_flow([{:+, n} | t], n, ep) do
    check_forward_flow(t, n + 1, ep)
  end

  def check_forward_flow([{:!, n} | t], n, n) do
    check_backward_flow(t, n)
  end

  def check_forward_flow(_, _, _), do: false

  def check_backward_flow([], _n), do: true

  def check_backward_flow([{:-, n} | t], n) do
    check_backward_flow(t, n - 1)
  end

  def check_backward_flow(_, _), do: false

  def compress_pipe(pipe) do
    pipe
    |> Enum.reduce(Fix.start([]), fn f, x -> f.(x) end)
    |> Fix.unwrap()
  end

  def pipe(n, error!)

  def pipe(n, nil) do
    build_pipe(n)
    |> Enum.map(fn {f, g} -> Fix.stage(f, g) end)
    |> then(&{nil, &1})
  end

  def pipe(n, error!) do
    build_pipe(n, error!)
    |> Enum.map(fn {f, g} -> Fix.stage(f, g) end)
    |> then(&{error!, &1})
  end

  def build_pipe(n, error! \\ nil)

  def build_pipe(n, nil) do
    Enum.map(1..n, fn x -> stage(:ok, x) end)
  end

  def build_pipe(n, error!) do
    Enum.map(1..n, fn x ->
      if x == error! do
        stage(:error, x)
      else
        stage(:ok, x)
      end
    end)
  end

  def build_stage(ok_or_error, n) do
    {f, g} = stage(ok_or_error, n)
    Fix.stage(f, g)
  end

  def stage(:ok, n) do
    {
      fn xs -> {:ok, [{:+, n} | xs]} end,
      fn r, _xs -> [{:-, n} | r] end
    }
  end

  def stage(:error, n) do
    {
      fn xs -> {:error, [{:!, n} | xs]} end,
      fn r, _xs -> [{:-, n} | r] end
    }
  end

  def stage(:bad_output, n) do
    {
      fn xs -> [{:!, n} | xs] end,
      fn r, _xs -> [{:-, n} | r] end
    }
  end
end
