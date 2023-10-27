defmodule PendDocTest do
  use ExUnit.Case, async: true
  doctest Pend, import: true
end

defmodule PendTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  property "bind/2 id chains work" do
    check all(
            ks <- list_of(bindable_id(), length: 1..10),
            a <- term(),
            s <- term()
          ) do
      m =
        Enum.reduce(ks, Pend.return(a), fn k, m ->
          Pend.bind(m, k)
        end)

      assert {^a, ^s} = Pend.run(m, s)
    end
  end

  property "return/1 returns value" do
    check all({a, s} <- tuple({term(), term()})) do
      assert {{:done, ^a}, _} = Pend.step(Pend.return(a), s)
    end
  end

  property "yield/1 suspends computation" do
    check all({a, s} <- tuple({term(), term()})) do
      assert {{:suspend, m}, ss} = Pend.step(Pend.yield(a), s)
      assert {{:done, ^a}, _} = Pend.step(m, ss)
    end
  end

  property "yield/1 w/ function may not return immediately" do
    check all({a, s} <- tuple({term(), term()})) do
      assert {{:suspend, m}, ss} = Pend.step(Pend.yield(Pend.yield(a)), s)
      assert {{:suspend, mm}, sss} = Pend.step(m, ss)
      assert {{:done, ^a}, _} = Pend.step(mm, sss)
    end
  end

  property "get/0 replaces a with s" do
    check all(
            a <- term(),
            s <- term()
          ) do
      m = a |> Pend.return() |> Pend.bind(Pend.get())
      assert {^s, ^s} = Pend.run(m, s)
    end
  end

  property "get/1 lenses a" do
    check all({a, s, aa} <- tuple({term(), term(), term()})) do
      m = a |> Pend.return() |> Pend.bind(Pend.get(fn _, _ -> aa end))
      assert {^aa, ^s} = Pend.run(m, s)
    end
  end

  property "update/1 lenses s" do
    check all({a, s, ss} <- tuple({term(), term(), term()})) do
      m = a |> Pend.return() |> Pend.bind(Pend.update(fn _, _ -> ss end))
      assert {^a, ^ss} = Pend.run(m, s)
    end
  end

  property "update/1 and put/1 are the same" do
    check all({a, s, ss} <- tuple({term(), term(), term()})) do
      m = a |> Pend.return() |> Pend.bind(Pend.put(ss))
      mm = a |> Pend.return() |> Pend.bind(Pend.update(fn _, _ -> ss end))
      assert Pend.run(m, s) == Pend.run(mm, s)
    end
  end

  property "get_and_update/1 works" do
    check all({a, s, aa, ss} <- tuple({term(), term(), term(), term()})) do
      {gua, gus} =
        a
        |> Pend.return()
        |> Pend.bind(Pend.get_and_update(fn _, _ -> {aa, ss} end))
        |> Pend.run(s)

      {ga, _gs} = a |> Pend.return() |> Pend.bind(Pend.get(fn _, _ -> aa end)) |> Pend.run(s)
      {_ua, us} = a |> Pend.return() |> Pend.bind(Pend.update(fn _, _ -> ss end)) |> Pend.run(s)

      assert gua == ga
      assert gus == us
    end
  end

  def bindable_id do
    member_of([
      Pend.return(),
      Pend.yield()
    ])
  end
end
