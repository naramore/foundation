defmodule MockingDocTest do
  use ExUnit.Case, async: true
  doctest Mocking, import: true
end

defmodule MockingTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import ExUnit.CaptureLog

  # accepting?/1
  # lang?/1

  # reduces/4
  # reduce/4
  # trace_verification/4

  # unambiguous?/1
  # reductions/1
  # consumes/1
  # consumes?/2
  # overlaps/1
  # overlaps?/2
  # alphabet/1
  # alphabet?/2

  describe "verification testing of" do
    @describetag property: :verification_testing

    #
  end

  describe "post-conditions of" do
    @describetag property: :post_conditions
  end

  describe "metamorphic testing of" do
    @describetag property: :metamorphic_testing
  end

  describe "inductive testing of" do
    @describetag property: :inductive_testing
  end

  describe "model-based testing of" do
    @describetag property: :model_based_testing
  end

  # TODO: add seq, alt, par list support???
  # TODO: to_list/1, from_list/1
  #       ts() :: [t()]
  #       t() :: {:alt, ts()} | {:par, ts()} | {:repl, t()} | atom() | nil | ts()

  # TODO: construct lang equivalence relataion
  #       + preservation checks (e.g. seq(seq(p, q), r) == seq(p, seq(q, r)))
  #                                   ^ same for par & alt

  # TODO: construct unambiguous lang (w/o filter?)
  # TODO: construct guarunteed ambiguous lang
  #       (i.e. construct valid -> inject ambiguity)

  # NOTE: add empty?
  def lang(a, r) do
    tree(
      event(a, r),
      fn x ->
        one_of([
          seq(x, x),
          par(x, x),
          alt(x, x),
          repl(x),
        ])
      end
    )
  end

  def seq(p, q), do: map({p, q}, fn {p, q} -> Mocking.seq(p, q) end)
  def par(p, q), do: map({p, q}, fn {p, q} -> Mocking.par(p, q) end)
  def alt(p, q), do: map({p, q}, fn {p, q} -> Mocking.alt(p, q) end)
  def repl(p), do: map(p, fn p -> Mocking.repl(p) end)
  def event(a, r), do: map({a, r}, fn {a, r} -> Mocking.event(a, r) end)
  def empty(), do: constant(Mocking.empty())
end
