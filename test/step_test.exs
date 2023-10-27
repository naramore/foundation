defmodule StepDocTest do
  use ExUnit.Case, async: true
  doctest Step, import: true
end

defmodule StepTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Step.{Hangman, RLE}

  describe "Step" do
    property "bind/2 id chains work" do
      check all(
              ks <- list_of(bindable_id(), length: 1..10),
              a <- term(),
              s <- term()
            ) do
        m =
          Enum.reduce(ks, Step.start(a), fn k, m ->
            Step.bind(m, k)
          end)

        assert {^a, ^s} = Step.run(m, s)
      end
    end

    property "map/2 binds normal function" do
      check all(
              f <- map(term(), fn b -> fn x -> {x, b} end end),
              {a, s} <- tuple({term(), term()})
            ) do
        assert {_, ^s} = a |> Step.start() |> Step.map(f) |> Step.run(s)
      end
    end

    property "step/2 id chains suspend" do
      check all(
              ks <- list_of(constant(nil), length: 1..10),
              {a, s} <- tuple({term(), term()})
            ) do
        m =
          Enum.reduce(ks, Step.start(a), fn _, m ->
            Step.bind(m, Pend.return())
          end)

        {m, s} =
          Enum.reduce(1..length(ks), {m, s}, fn _, {m, s} ->
            assert {{:suspend, m}, s} = Step.step(m, s)
            {m, s}
          end)

        assert {{:done, ^a}, ^s} = Step.step(m, s)
      end
    end

    property "start/1 returns value" do
      check all({a, s} <- tuple({term(), term()})) do
        assert {{:done, ^a}, _} = Step.step(Step.start(a), s)
      end
    end

    property "Pend.yield/1 suspends computation" do
      check all({a, s} <- tuple({term(), term()})) do
        assert {{:suspend, m}, ss} = Step.step(Pend.yield(a), s)
        assert {{:done, ^a}, _} = Step.step(m, ss)
      end
    end

    property "Pend.yield/1 w/ function may not return immediately" do
      check all({a, s} <- tuple({term(), term()})) do
        assert {{:suspend, m}, ss} = Step.step(Pend.yield(Pend.yield(a)), s)
        assert {{:suspend, mm}, sss} = Step.step(m, ss)
        assert {{:done, ^a}, _} = Step.step(mm, sss)
      end
    end
  end

  describe "Step.Reductions" do
    property "Step.RLE should return valid output" do
      check all(a <- string(:alphanumeric, length: 0..100)) do
        m =
          String.codepoints(a)
          |> then(&{:rle, &1})
          |> Step.start()
          |> RLE.bind()

        assert {{:done, result}, nil} = Step.run(m, nil)
        assert check_rle(result)
      end
    end

    test "Step.RLE end-to-end" do
      assert {{:done, result}, nil} =
               String.codepoints("aabccccdeeffffff")
               |> then(&{:rle, &1})
               |> Step.start()
               |> RLE.bind()
               |> Step.run(nil)

      assert result == [{"a", 2}, "b", {"c", 4}, "d", {"e", 2}, {"f", 6}]
    end

    test "Step.Hangman end-to-end win" do
      m = Step.HangmanModel.build(word: "octopus", turns: 10)

      assert {{:the_word, "_______"}, m} = Hangman.run(:word_so_far, m)
      assert {:bad_guess, m} = Hangman.run({:make_move, "a"}, m)
      assert {{:the_word, "_______"}, m} = Hangman.run(:word_so_far, m)
      assert {:good_guess, m} = Hangman.run({:make_move, "o"}, m)
      assert {{:the_word, "o__o___"}, m} = Hangman.run(:word_so_far, m)
      assert {:already_tried, m} = Hangman.run({:make_move, "o"}, m)
      assert {{:the_word, "o__o___"}, m} = Hangman.run(:word_so_far, m)
      assert {:good_guess, m} = Hangman.run({:make_move, "s"}, m)
      assert {{:the_word, "o__o__s"}, m} = Hangman.run(:word_so_far, m)
      assert {:good_guess, m} = Hangman.run({:make_move, "t"}, m)
      assert {{:the_word, "o_to__s"}, m} = Hangman.run(:word_so_far, m)
      assert {:good_guess, m} = Hangman.run({:make_move, "p"}, m)
      assert {{:the_word, "o_top_s"}, m} = Hangman.run(:word_so_far, m)
      assert {:good_guess, m} = Hangman.run({:make_move, "u"}, m)
      assert {{:the_word, "o_topus"}, m} = Hangman.run(:word_so_far, m)
      assert {:game_won, _} = Hangman.run({:make_move, "c"}, m)
    end
  end

  def check_rle([]), do: true
  def check_rle([{s, n} | t]) when is_binary(s) and is_integer(n), do: check_rle(t)
  def check_rle([s | t]) when is_binary(s), do: check_rle(t)
  def check_rle(_), do: false

  def bindable_id do
    member_of([
      Pend.return(),
      Pend.yield()
    ])
  end
end
