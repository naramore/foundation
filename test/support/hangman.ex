defmodule Step.HangmanModel do
  @moduledoc false

  defstruct word: nil,
            turns: nil,
            guesses: [" "]

  @type t :: %__MODULE__{
          word: String.t(),
          turns: integer(),
          guesses: [letter()]
        }

  @type letter :: String.t()

  @spec build(keyword()) :: t()
  def build(opts) do
    %__MODULE__{
      word: Keyword.get(opts, :word),
      turns: Keyword.get(opts, :turns, 10)
    }
  end

  @spec use_letter(t(), letter()) :: t()
  def use_letter(model, letter) do
    Map.update!(model, :guesses, &[letter | &1])
  end

  @spec one_less_turn(t()) :: t()
  def one_less_turn(model) do
    Map.update!(model, :turns, &(&1 - 1))
  end

  @spec already_used_letter?(t(), letter()) :: bool()
  def already_used_letter?(model, letter) do
    letter in model.guesses
  end

  @spec letter_in_word?(t(), letter()) :: bool()
  def letter_in_word?(model, letter) do
    String.contains?(model.word, letter)
  end

  @spec game_won?(t()) :: bool()
  def game_won?(model) do
    model.word
    |> String.split("", trim: true)
    |> Enum.all?(&(&1 in model.guesses))
  end

  @spec out_of_turns?(t()) :: bool()
  def out_of_turns?(model) do
    model.turns <= 0
  end

  @spec word_so_far(t()) :: String.t()
  def word_so_far(model) do
    model.word
    |> String.split("", trim: true)
    |> Enum.map_join(fn x ->
      if x in model.guesses, do: x, else: "_"
    end)
  end
end

defmodule Step.Hangman do
  @moduledoc false
  use Step.Reductions,
    model: Step.HangmanModel,
    as: HM

  reductions(debug: false, model_name: :game) do
    {:make_move, move} ->
      {:make_move, move, HM.already_used_letter?(game, move)}

    {:make_move, _move, true} ->
      :already_tried

    {:make_move, move, _} ->
      {:record_move, move}

    {:record_move, move} ->
      update_model(HM.use_letter(game, move)) do
        {:maybe_match, move}
      end

    {:maybe_match, move} ->
      {:maybe_match, move, HM.letter_in_word?(game, move)}

    {:maybe_match, _move, true} ->
      :match

    {:maybe_match, _move, _} ->
      :no_match

    :match ->
      {:match, HM.game_won?(game)}

    {:match, true} ->
      :game_won

    {:match, _} ->
      :good_guess

    :no_match ->
      {:no_match, HM.out_of_turns?(game)}

    {:no_match, true} ->
      :game_lost

    {:no_match, _} ->
      update_model(HM.one_less_turn(game)) do
        :bad_guess
      end

    :word_so_far ->
      {:the_word, HM.word_so_far(game)}
  end
end
