defmodule FixDocTest do
  use ExUnit.Case, async: true
  doctest Fix, import: true
end

defmodule FixTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  property "id/1 is a no-op" do
    check all(x <- term()) do
      assert x == Fix.id(x)
    end
  end

  property "id/2 is a no-op" do
    check all({reason, x} <- tuple({term(), term()})) do
      assert reason == Fix.id(reason, x)
    end
  end

  property "start/1 is idempotent" do
    check all(x <- term()) do
      assert Fix.start(Fix.start(x)) == Fix.start(x)
    end
  end

  property "unwrap/1 undoes wrap/1" do
    check all(x <- term()) do
      assert Fix.unwrap(Fix.wrap(x)) == x
    end
  end

  property "unwrap/1 undoes start/1" do
    check all(x <- result()) do
      assert Fix.unwrap(Fix.start(x)) == x
    end
  end

  property "unwrap/1 sort of undoes start/1" do
    check all(x <- not_result()) do
      assert Fix.unwrap(Fix.start(x)) == {:ok, x}
    end
  end

  property "unwrap/1 does nothing if not wrapped" do
    check all(x <- filter(term(), &(not match?({:__fix__, _, _}, &1)))) do
      assert x == Fix.unwrap(x)
    end
  end

  property "stage/2 auto-wraps unwrapped input" do
    check all(
            a <- member_of([:ok, :error]),
            n <- integer()
          ) do
      assert {:__fix__, _, _} = Fix.Generators.build_stage(a, n).([])
    end
  end

  property "stage/2 auto-wraps non-result input" do
    check all(n <- integer()) do
      assert {:error, {:unsupported_result, _}} =
               Fix.Generators.build_stage(:bad_output, n).(Fix.wrap([]))
               |> Fix.unwrap()
    end
  end

  property "stage/2 raises on non-result output" do
    check all(n <- integer()) do
      assert_raise(CaseClauseError, fn ->
        Fix.Generators.build_stage(:bad_output, n).([])
        |> Fix.unwrap()
      end)
    end
  end

  property "stage/2 pipes and succeeds" do
    check all({_error_point, pipe} <- pipe(1..10, false)) do
      assert {:ok, result = [_ | _]} = Fix.Generators.compress_pipe(pipe)
      assert Fix.Generators.check_forward_flow(Enum.reverse(result), 1, nil)
    end
  end

  property "stage/2 pipes and compensates" do
    check all({error_point, pipe} <- pipe(1..10, true)) do
      assert {:error, result = [_ | _]} = Fix.Generators.compress_pipe(pipe)
      assert Fix.Generators.check_forward_flow(Enum.reverse(result), 1, error_point)
    end
  end

  def pipe(range, error?)

  def pipe(range, true) do
    gen all(
          n <- integer(range),
          error! <- member_of(1..n)
        ) do
      Fix.Generators.pipe(n, error!)
    end
  end

  def pipe(range, false) do
    gen all(n <- integer(range)) do
      Fix.Generators.pipe(n, nil)
    end
  end

  def not_result do
    filter(term(), &(not match?({:ok, _}, &1) and not match?({:error, _}, &1)))
  end

  def result, do: result(term(), term())

  def result(a, b) do
    one_of([
      tuple({constant(:ok), a}),
      tuple({constant(:error), b})
    ])
  end
end
