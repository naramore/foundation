defmodule Mocking do
  @moduledoc """
  """
  use Boundary, deps: [], exports: []

  @typedoc """
  """
  @type action() :: {module(), atom(), [...]}

  @typedoc """
  """
  @type call(a) :: {:call, a}
  @type call() :: call(action())

  @typedoc """
  """
  @type error(call, result) ::
    {:expected, call}
    | {:expected_one_of, [call]}
    | {:unexpected, call}
    | {:unexpected, call, :expected, call}
    | {:unexpected, call, :expected_one_of, [call]}
    | {:unexpected_return, result, :expected, {call, :->, result}}
    | {:ambiguous, call, :cant_choose_between, [{call, :->, result}]}

  @type match_fun(a) :: (a, a -> boolean())
  @type result() :: term()
  @type seq_flag() :: :left | :right
  @type alt_flag() :: seq_flag() | :neither

  @typedoc """
  """
  @type seq(p, q) :: {:seq, seq_flag(), p, q}

  @typedoc """
  """
  @type par(p, q) :: {:par, p, q}

  @typedoc """
  """
  @type alt(p, q) :: {:alt, alt_flag(), p, q}

  @typedoc """
  """
  @type repl(p) :: {:repl, boolean(), p}

  @typedoc """
  """
  @type event(a, r) :: {:event, {:ok, r} | nil, a, r}

  @typedoc """
  """
  @type empty() :: :empty

  @typedoc """
  """
  @type lang(a, r) ::
    seq(lang(a, r), lang(a, r))
    | par(lang(a, r), lang(a, r))
    | alt(lang(a, r), lang(a, r))
    | repl(lang(a, r))
    | event(a, r)
    | empty()
  @type lang(a) :: lang(a, result())
  @type lang() :: lang(action())

  @doc """
  """
  @spec empty() :: lang(a, r) when a: term(), r: term()
  def empty, do: :empty

  @doc """
  """
  @spec seq([lang(a, r)]) :: lang(a, r) when a: term(), r: term()
  def seq(ps)
  def seq([p, q | []]), do: seq(p, q)
  def seq([p, q | t]), do: seq(p, seq([q | t]))

  @doc """
  """
  @spec seq(lang(a, r) | nil, lang(a, r) | nil, seq_flag()) :: lang(a, r) | nil when a: term(), r: term()
  def seq(p, q, flag \\ :left)
  def seq(nil, _, _), do: nil
  def seq(_, nil, _), do: nil
  def seq(p, q, flag), do: {:seq, flag, p, q}

  @doc """
  """
  @spec par([lang(a, r)]) :: lang(a, r) when a: term(), r: term()
  def par(ps)
  def par([p, q | []]), do: par(p, q)
  def par([p, q | t]), do: par(p, par([q | t]))

  @doc """
  """
  @spec par(lang(a, r) | nil, lang(a, r) | nil) :: lang(a, r) | nil when a: term(), r: term()
  def par(p, q)
  def par(nil, _), do: nil
  def par(_, nil), do: nil
  def par(p, q), do: {:par, p, q}

  @doc """
  """
  @spec alt(lang(a, r) | nil, lang(a, r) | nil, alt_flag()) :: lang(a, r) | nil when a: term(), r: term()
  def alt(p, q, flag \\ :neither)
  def alt(nil, _, _), do: nil
  def alt(_, nil, _), do: nil
  def alt(p, q, flag), do: {:alt, flag, p, q}

  @doc """
  """
  @spec repl(lang(a, r) | nil, boolean()) :: lang(a, r) | nil when a: term(), r: term()
  def repl(p, flag \\ false)
  def repl(nil, _), do: nil
  def repl(p, flag), do: {:repl, flag, p}

  @doc """
  """
  @spec event(a, r, r) :: lang(a, r) when a: term(), r: term()
  def event(a, r, flag \\ nil)
  def event(a, r, nil), do: {:event, nil, a, r}
  def event(a, r, result), do: {:event, {:ok, result}, a, r}

  @doc false
  @spec reset(lang(a, r)) :: lang(a, r) when a: term(), r: term()
  def reset(p)
  def reset({:seq, _, p, q}), do: seq(reset(p), reset(q))
  def reset({:par, _, p, q}), do: par(reset(p), reset(q))
  def reset({:alt, _, p, q}), do: alt(reset(p), reset(q))
  def reset({:repl, _, p}), do: repl(reset(p))
  def reset({:event, _, a, r}), do: event(a, r)
  def reset(:empty), do: empty()

  @doc """
  """
  @spec lang?(term()) :: boolean()
  def lang?(p)
  def lang?(:empty), do: true
  def lang?({:event, nil, _, _}), do: true
  def lang?({:event, {:ok, _}, _, _}), do: true
  def lang?({:repl, f, p}) when is_boolean(f), do: lang?(p)
  def lang?({:seq, f, p, q}) when f in [:left, :right], do: lang?(p) and lang?(q)
  def lang?({:par, p, q}), do: lang?(p) and lang?(q)
  def lang?({:alt, f, p, q}) when f in [:neither, :left, :right], do: lang?(p) and lang?(q)
  def lang?(_), do: false

  @doc """
  """
  @spec accepting?(lang(a, r)) :: boolean() when a: term(), r: term()
  def accepting?(p)
  def accepting?({:seq, :left, p, q}), do: accepting?(p) and accepting?(q)
  def accepting?({:seq, :right, _, q}), do: accepting?(q)
  def accepting?({:par, p, q}), do: accepting?(p) and accepting?(q)
  def accepting?({:alt, :neither, p, q}), do: accepting?(p) or accepting?(q)
  def accepting?({:alt, :left, p, _}), do: accepting?(p)
  def accepting?({:alt, :right, _, q}), do: accepting?(q)
  def accepting?({:repl, _, _}), do: true
  def accepting?({:event, _, _, _}), do: false
  def accepting?(:empty), do: true

  @doc """
  """
  @spec reduces(lang(a, r), [], match_fun(a), (a, r -> r)) ::
    {:ok, [r]}
    | {:error, {:expected, a}}
    | {:error, {:expected_one_of, [a]}}
    | {:error, {:unexpected, a}}
    | {:error, {:unexpected, a, :expected, a}}
    | {:error, {:unexpected, a, :expected_one_of, [a]}}
    | {:error, {:unexpected_return, r, :expected, {a, :->, r}}}
    | {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}]}}
    when a: term(), r: term()
  def reduces(p, as, matcher \\ &matcher/2, caller \\ &noop/2) do
    Enum.reduce_while(as, {[], p}, fn a, {rs, q} ->
      case reduce(q, a, matcher, caller) do
        {:ok, r, qq} ->
          {:cont, {[r | rs], qq}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:error, reason} -> {:error, reason}
      {rs, q} ->
        if accepting?(q) do
          {:ok, Enum.reverse(rs)}
        else
          case Enum.into(consumes(q), []) do
            [a] -> {:error, {:expected, a}}
            [_ | _] = as -> {:error, {:expected_one_of, as}}
          end
        end
    end
  end

  @doc """
  """
  @spec reduce(lang(a, r), a, match_fun(a), (a, r -> r)) ::
    {:ok, r, lang(a, r)}
    | {:error, {:unexpected, a}}
    | {:error, {:unexpected, a, :expected, a}}
    | {:error, {:unexpected, a, :expected_one_of, [a]}}
    | {:error, {:unexpected_return, r, :expected, {a, :->, r}}}
    | {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}]}}
    when a: term(), r: term()
  def reduce(p, a, matcher \\ &matcher/2, caller \\ &noop/2)
  def reduce({:seq, :left, p, q}, a, m, c) do
    case reduce(p, a, m, c) do
      {:ok, r, x} ->
        case reduce(q, a, m, &noop/2) do
          {:ok, qr, _} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}, {a, :->, qr}]}}
          {:error, {:ambiguous, ^a, _, cs}} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r} | cs]}}
          {:error, _} -> {:ok, r, seq(x, q)}
        end

      {:error, reason} ->
        if accepting?(p) do
          reduce(seq(p, q, :right), a , m, c)
        else
          case Enum.into(consumes(p), []) do
            [aa] -> {:error, {:unexpected, a, :expected, aa}}
            [_ | _] = as -> {:error, {:unexpected, a, :expected_one_of, as}}
          end
        end
        |> maybe_merge_errors(reason)
    end
  end
  def reduce({:seq, :right, p, q}, a, m, c) do
    case reduce(q, a, m, c) do
      {:error, reason} -> {:error, reason}
      {:ok, r, x} -> {:ok, r, seq(p, x, :right)}
    end
  end
  def reduce({:par, p, q}, a, m, c) do
    case reduce(p, a, m, c) do
      {:ok, r, x} ->
        case reduce(q, a, m, &noop/2) do
          {:ok, qr, _} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}, {a, :->, qr}]}}
          {:error, {:ambiguous, ^a, _, cs}} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r} | cs]}}
          {:error, _} -> {:ok, r, par(x, q)}
        end

      {:error, {:ambiguous, _, _, _} = reason} ->
        {:error, reason}

      {:error, reason} ->
        case reduce(q, a, m, c) do
          {:ok, r, x} -> {:ok, r, par(p, x)}
          {:error, reason2} -> {:error, merge_errors(reason2, reason)}
        end
    end
  end
  def reduce({:alt, :neither, p, q}, a, m, c) do
    case reduce(p, a, m, c) do
      {:ok, r, x} ->
        case reduce(q, a, m, &noop/2) do
          {:ok, qr, _} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}, {a, :->, qr}]}}
          {:error, {:ambiguous, ^a, _, cs}} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r} | cs]}}
          {:error, _} -> {:ok, r, alt(x, q, :left)}
        end

      {:error, {:ambiguous, _, _, _} = reason} ->
        {:error, reason}

      {:error, reason} ->
        case reduce(q, a, m, c) do
          {:ok, r, x} -> {:ok, r, alt(p, x, :right)}
          {:error, reason2} -> {:error, merge_errors(reason, reason2)}
        end
    end
  end
  def reduce({:alt, :left, p, q}, a, m, c) do
    case reduce(p, a, m, c) do
      {:error, reason} -> {:error, reason}
      {:ok, x, r} ->
        case reduce(q, a, m, &noop/2) do
          {:ok, qr, _} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}, {a, :->, qr}]}}
          {:error, {:ambiguous, ^a, _, cs}} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r} | cs]}}
          {:error, _} -> {:ok, r, alt(x, q, :left)}
        end
    end
  end
  def reduce({:alt, :right, p, q}, a, m, c) do
    case reduce(q, a, m, c) do
      {:ok, r, x} -> {:ok, r, alt(p, x, :right)}
      {:error, reason} -> {:error, reason}
    end
  end
  def reduce({:repl, _, p}, a, m, c) do
    case reduce(p, a, m, c) do
      {:error, reason} -> {:error, reason}
      {:ok, r, x} ->
        case reduce(p, a, m, &noop/2) do
          {:ok, pr, _} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r}, {a, :->, pr}]}}
          {:error, {:ambiguous, ^a, _, cs}} -> {:error, {:ambiguous, a, :cant_choose_between, [{a, :->, r} | cs]}}
          {:error, _} -> {:ok, r, seq(x, repl(p, true))}
        end
    end
  end
  def reduce({:event, nil, a, r}, a, m, c) do
    reduce(event(a, r, c.(a, r)), a, m, c)
  end
  def reduce({:event, {:ok, rr}, a, r}, a, m, _) do
    if m.(r, rr) do
      {:ok, rr, empty()}
    else
      {:error, {:unexpected_return, rr, :expected, {a, :->, r}}}
    end
  end
  def reduce(:empty, a, _, _) do
    {:error, {:unexpected, a}}
  end
  def reduce(p, a, _, _) do
    case Enum.into(consumes(p), []) do
      [] -> {:error, {:unexpected, a}}
      [aa] -> {:error, {:unexpected, a, :expected, aa}}
      [_ | _] = as -> {:error, {:unexpected, a, :expected_one_of, as}}
    end
  end

  @spec maybe_merge_errors({:ok, r, lang(a, r)} | {:error, error(a, r)}, error(a, r)) :: {:ok, r, lang(a, r)} | {:error, error(a, r)} when a: term(), r: term()
  defp maybe_merge_errors({:ok, r, x}, _), do: {:ok, r, x}
  defp maybe_merge_errors({:error, e1}, e2), do: {:error, merge_errors(e1, e2)}

  @spec merge_errors(error(a, r), error(a, r)) :: error(a, r) when a: term(), r: term()
  defp merge_errors(e, e), do: e
  defp merge_errors({:ambiguous, a, :cant_choose_between, bs}, {:ambiguous, a, :cant_choose_between, cs}), do: {:ambiguous, a, :cant_choose_between, bs ++ cs}
  defp merge_errors({:unexpected, a, :expected, b}, {:unexpected, a, :expected, c}), do: {:unexpected, a, :expected_one_of, [b, c]}
  defp merge_errors({:unexpected, a, :expected, b}, {:unexpected, a, :expected_one_of, cs}), do: {:unexpected, a, :expected_one_of, [b | cs]}
  defp merge_errors({:unexpected, a, :expected_one_of, bs}, {:unexpected, a, :expected, c}), do: {:unexpected, a, :expected_one_of, bs ++ [c]}
  defp merge_errors({:unexpected, a, :expected_one_of, bs}, {:unexpected, a, :expected_one_of, cs}), do: {:unexpected, a, :expected_one_of, bs ++ cs}
  defp merge_errors({:unexpected, _, :expected, _} = e, _), do: e
  defp merge_errors(_, {:unexpected, _, :expected, _} = e), do: e
  defp merge_errors({:unexpected, _, :expected_one_of, _} = e, _), do: e
  defp merge_errors(_, {:unexpected, _, :expected_one_of, _} = e), do: e
  defp merge_errors(e, _), do: e

  @doc false
  @spec matcher(r, r) :: boolean() when r: term()
  def matcher(r1, r2), do: r1 == r2

  @doc false
  @spec noop(a, r) :: r when a: term(), r: term()
  def noop(_a, r), do: r

  @doc """
  """
  @spec trace_verification(lang(a, r), [a], match_fun(a), (a, r -> r)) :: boolean() when a: term(), r: term()
  def trace_verification(lang, actions, matcher \\ &matcher/2, caller \\ &noop/2) do
    match?({:ok, _}, reduces(lang, actions, matcher, caller))
  end

  @doc """
  """
  @spec unambiguous?(lang(a, r)) :: boolean() when a: term(), r: term()
  def unambiguous?(:empty), do: true
  def unambiguous?({:event, _, _, _}), do: true
  def unambiguous?({:alt, _, p, q}), do: unambiguous?(p) and unambiguous?(q) and MapSet.disjoint?(consumes(p), consumes(q))
  def unambiguous?({:par, p, q}), do: unambiguous?(p) and unambiguous?(q) and MapSet.disjoint?(alphabet(p), alphabet(q))
  def unambiguous?({:seq, _, p, q}), do: unambiguous?(p) and unambiguous?(q) and MapSet.disjoint?(overlaps(p), consumes(q))
  def unambiguous?({:repl, _, p}), do: unambiguous?(p) and Enum.all?(reductions(p), &MapSet.disjoint?(overlaps(&1), consumes(p)))

  @doc """
  """
  @spec reductions(lang(a, r)) :: [lang(a, r)] when a: term(), r: term()
  def reductions(p) do
    consumes(p)
    |> Stream.map(fn a -> elem(reduce(p, a, &matcher/2, &noop/2), 2) end)
    |> Stream.flat_map(&[&1 | reductions(&1)])
    |> Enum.to_list()
  end

  @doc """
  """
  @spec consumes(lang(a, r)) :: MapSet.t(a) when a: term(), r: term()
  def consumes(p), do: Enum.filter(alphabet(p), &consumes?(p, &1)) |> MapSet.new()

  @doc """
  """
  @spec consumes?(lang(a, r), a) :: boolean() when a: term(), r: term()
  def consumes?(p, a), do: match?({:ok, _, _}, reduce(p, a, &matcher/2, &noop/2))

  @doc """
  """
  @spec overlaps(lang(a, r)) :: MapSet.t(a) when a: term(), r: term()
  def overlaps(p), do: Enum.filter(alphabet(p), &overlaps?(p, &1)) |> MapSet.new()

  @doc """
  """
  @spec overlaps?(lang(a, r), a) :: boolean() when a: term(), r: term()
  def overlaps?(p, a)
  def overlaps?(:empty, _), do: false
  def overlaps?({:event, _, _, _}, _), do: false
  def overlaps?({:par, p, q}, a), do: overlaps?(p, a) or overlaps?(q, a)
  def overlaps?({:alt, _, p, q}, a), do: overlaps?(p, a) or overlaps?(q, a) or (consumes?(p, a) and accepting?(q)) or (accepting?(p) and consumes?(q, a))
  def overlaps?({:seq, _, p, q}, a), do: overlaps?(q, a) or (overlaps?(p, a) and accepting?(q))
  def overlaps?({:repl, _, p}, a), do: overlaps?(p, a) or consumes?(p, a)

  @doc """
  """
  @spec alphabet(lang(a, r)) :: MapSet.t(a) when a: term(), r: term()
  def alphabet(p), do: do_alphabet(p, []) |> Enum.map(&elem(&1, 0)) |> MapSet.new()

  @spec do_alphabet(lang(a, r), [a]) :: [{a, r}] when a: term(), r: term()
  defp do_alphabet({:event, _, a, r}, acc), do: [{a, r} | acc]
  defp do_alphabet({:repl, _, p}, acc), do: do_alphabet(p, acc)
  defp do_alphabet({:seq, _, p, q}, acc), do: do_alphabet(q, do_alphabet(p, acc))
  defp do_alphabet({:par, p, q}, acc), do: do_alphabet(q, do_alphabet(p, acc))
  defp do_alphabet({:alt, _, p, q}, acc), do: do_alphabet(q, do_alphabet(p, acc))
  defp do_alphabet(:empty, acc), do: acc

  @doc """
  """
  @spec alphabet?(lang(a, r), a) :: boolean() when a: term(), r: term()
  def alphabet?({:event, _, a, _}, a), do: true
  def alphabet?({:repl, _, p}, a), do: alphabet?(p, a)
  def alphabet?({:seq, _, p, q}, a), do: alphabet?(p, a) or alphabet?(q, a)
  def alphabet?({:par, p, q}, a), do: alphabet?(p, a) or alphabet?(q, a)
  def alphabet?({:alt, _, p, q}, a), do: alphabet?(p, a) or alphabet?(q, a)
  def alphabet?(_, _), do: false
end
