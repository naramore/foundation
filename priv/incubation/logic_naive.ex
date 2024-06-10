defmodule Logic.Naive do
  use Boundary, deps: [], exports: []

  @type t(a) :: list(a)
  @type goal(a, b) :: (a -> t(b))
  @type goal(a) :: goal(a, a)

  @spec succeed(a) :: t(a) when a: any()
  def succeed(a), do: [a]
  @spec succeed() :: goal(a) when a: any()
  def succeed, do: &succeed/1

  @spec fail(a) :: t(a) when a: any()
  def fail(_), do: []
  @spec fail() :: goal(a) when a: any()
  def fail, do: &fail/1

  @spec other((goal(a) -> goal(a)), goal(a), a) :: t(a) when a: any()
  def other(g, f, x), do: g.(f, x)
  @spec other((goal(a) -> goal(a)), goal(a)) :: goal(a) when a: any()
  def other(g, f), do: &other(g, f, &1)

  @spec condu(goal(a), goal(a), goal(a), a) :: t(a) when a: any()
  def condu(g, h, f, x) do
    case g.(x) do
      [] -> f.(x)
      [y | _] -> h.(y)
    end
  end
  @spec condu(goal(a), goal(a), goal(a)) :: goal(a) when a: any()
  def condu(g, h, f), do: &condu(g, h, f, &1)
  @spec condu(goal(a), goal(a)) :: (goal(a) -> goal(a)) when a: any()
  def condu(g, h), do: &condu(g, h, &1)
  @spec condu([[goal(a), ...], ...]) :: goal(a) when a: any()
  def condu([]), do: fail()
  def condu([[g | gs] | gss]), do: g |> condu(all(gs)) |> other(condu(gss))

  # sequential
  #############################################################################

  @spec mplus(t(a), t(a)) :: t(a) when a: any()
  def mplus([], ys), do: ys
  def mplus([x | xs], ys), do: [x | mplus(xs, ys)]

  @spec bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
  def bind([], _k), do: []
  def bind([x | xs], k), do: mplus(k.(x), bind(xs, k))

  @spec conde(goal(a), goal(a), a) :: t(a) when a: any()
  def conde(f, g, x), do: mplus(f.(x), g.(x))
  @spec conde(goal(a), goal(a)) :: goal(a) when a: any()
  def conde(f, g), do: &conde(f, g, &1)
  @spec conde([[goal(a), ...], ...]) :: goal(a) when a: any()
  def conde([gs]), do: all(gs)
  def conde([gs | gss]), do: conde(all(gs), conde(gss))

  @spec all(goal(a), goal(a), a) :: t(a) when a: any()
  def all(f, g, x), do: bind(f.(x), g)
  @spec all(goal(a), goal(a)) :: goal(a) when a: any()
  def all(f, g), do: &all(f, g, &1)
  @spec all([[goal(a), ...], ...]) :: goal(a) when a: any()
  def all([g]), do: g
  def all([g | gs]), do: all(g, all(gs))

  @spec conda(goal(a), goal(a), goal(a), a) :: t(a) when a: any()
  def conda(g, h, f, x) do
    case g.(x) do
      [] -> f.(x)
      ys -> bind(ys, h)
    end
  end
  @spec conda(goal(a), goal(a), goal(a)) :: goal(a) when a: any()
  def conda(g, h, f), do: &conda(g, h, f, &1)
  @spec conda(goal(a), goal(a)) :: (goal(a) -> goal(a)) when a: any()
  def conda(g, h), do: &conda(g, h, &1)
  @spec conda([[goal(a), ...], ...]) :: goal(a) when a: any()
  def conda([]), do: fail()
  def conda([[g | gs] | gss]), do: g |> conda(all(gs)) |> other(conda(gss))

  # alternating
  #############################################################################

  @spec mplusi(t(a), t(a)) :: t(a) when a: any()
  def mplusi([], ys), do: ys
  def mplusi([x | xs], ys), do: [x | mplusi(ys, xs)]

  @spec bindi(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
  def bindi([], _k), do: []
  def bindi([x | xs], k), do: mplusi(k.(x), bindi(xs, k))

  @spec condi(goal(a), goal(a), a) :: t(a) when a: any()
  def condi(f, g, x), do: mplus(f.(x), g.(x))
  @spec condi(goal(a), goal(a)) :: goal(a) when a: any()
  def condi(f, g), do: &condi(f, g, &1)
  @spec condi([[goal(a), ...], ...]) :: goal(a) when a: any()
  def condi([gs]), do: all(gs)
  def condi([gs | gss]), do: condi(all(gs), condi(gss))

  @spec alli(goal(a), goal(a), a) :: t(a) when a: any()
  def alli(f, g, x), do: bindi(f.(x), g)
  @spec alli(goal(a), goal(a)) :: goal(a) when a: any()
  def alli(f, g), do: &alli(f, g, &1)
  @spec alli([[goal(a), ...], ...]) :: goal(a) when a: any()
  def alli([g]), do: g
  def alli([g | gs]), do: alli(g, alli(gs))

  @spec condai(goal(a), goal(a), goal(a), a) :: t(a) when a: any()
  def condai(g, h, f, x) do
    case g.(x) do
      [] -> f.(x)
      ys -> bindi(ys, h)
    end
  end
  @spec condai(goal(a), goal(a), goal(a)) :: goal(a) when a: any()
  def condai(g, h, f), do: &condai(g, h, f, &1)
  @spec condai(goal(a), goal(a)) :: (goal(a) -> goal(a)) when a: any()
  def condai(g, h), do: &condai(g, h, &1)
  @spec condai([[goal(a), ...], ...]) :: goal(a) when a: any()
  def condai([]), do: fail()
  def condai([[g | gs] | gss]), do: g |> condai(all(gs)) |> other(condai(gss))
end
