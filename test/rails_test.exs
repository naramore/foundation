defmodule RailsDocTest do
  use ExUnit.Case, async: true
  doctest Rails, import: true
end

defmodule RailsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import ExUnit.CaptureLog

  # TODO: split/4, plus/4

  describe "verification testing of" do
    @describetag property: :verification_testing

    property "two_track/2" do
      check all(tt <- two_track(term(), term())) do
        assert_two_track(tt)
      end
    end

    property "one_track_fun/1" do
      check all(
              a <- not_two_track(),
              otf <- one_track_fun(term())
            ) do
        otf.(a)
      end
    end

    property "switch_fun/2" do
      check all(
              a <- not_two_track(),
              sf <- switch_fun(term(), term())
            ) do
        sf.(a)
      end
    end

    property "two_track_fun/2" do
      check all(
              tti <- two_track(term(), term()),
              sf <- two_track_fun(term(), term())
            ) do
        sf.(tti)
      end
    end

    property "tee/0" do
      check all(
              a <- term(),
              t <- tee()
            ) do
        assert :ok = t.(a)
      end
    end

    %{
      {:one_track, :one_track} => :ok,
      {:one_track, :switch} => :ok,
      {:one_track, :two_track} => :bad,
      {:switch, :one_track} => :bad,
      {:switch, :switch} => :bad,
      {:switch, :two_track} => :ok,
      {:two_track, :one_track} => :bad,
      {:two_track, :switch} => :bad,
      {:two_track, :two_track} => :ok
    }
    |> Enum.map(fn {{a, b}, r} ->
      @a a
      @b b
      @result r
      property "#{@a} |> #{@b} is #{@result}" do
        check all(
                x <- val(@a),
                f <- fun(@a, term(), term()),
                g <- fun(@b, term(), term())
              ) do
          case @result do
            :ok ->
              x |> f.() |> g.()

            :bad ->
              assert_raise ArgumentError, fn ->
                x |> f.() |> g.()
              end
          end
        end
      end
    end)
  end

  property "switch/1 success" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:ok, _} = Rails.switch(otf).(a)
    end
  end

  property "bind/1 success" do
    check all(
            a <- term(),
            sf <- switch_fun(term(), term())
          ) do
      assert_two_track(Rails.bind(sf).({:ok, a}))
    end
  end

  property "bind/1 failure pass-through" do
    check all(
            a <- term(),
            sf <- switch_fun(term(), term())
          ) do
      assert {:error, ^a} = Rails.bind(sf).({:error, a})
    end
  end

  property "map/1 success" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:ok, _} = Rails.map(otf).({:ok, a})
    end
  end

  property "map/1 failure pass-through" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:error, ^a} = Rails.map(otf).({:error, a})
    end
  end

  property "fmap/1 success pass-through" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:ok, ^a} = Rails.fmap(otf).({:ok, a})
    end
  end

  property "fmap/1 failure" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:error, _} = Rails.fmap(otf).({:error, a})
    end
  end

  property "tee/1 ok" do
    check all(
            a <- not_two_track(),
            otf <- tee()
          ) do
      assert a == Rails.tee(otf).(a)
    end
  end

  property "safe/1 success" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun(term())
          ) do
      assert {:ok, _} = Rails.safe(otf).(a)
    end
  end

  property "safe/1 raise" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun_raise()
          ) do
      assert {:error, _} = Rails.safe(otf).(a)
    end
  end

  property "safe/1 throw" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun_throw()
          ) do
      assert {:error, {:caught, _}} = Rails.safe(otf).(a)
    end
  end

  property "safe/1 exit" do
    check all(
            a <- not_two_track(),
            otf <- one_track_fun_exit()
          ) do
      assert {:error, {:exit, _}} = Rails.safe(otf).(a)
    end
  end

  property "wrap/1 already wrapped" do
    check all(x <- two_track(term(), term())) do
      assert x == Rails.wrap(x)
    end
  end

  property "wrap/1 not wrapped" do
    check all(x <- not_two_track()) do
      wrapped = Rails.wrap(x)
      assert_two_track(wrapped)
      refute x == wrapped
    end
  end

  property "unwrap!/1 ok" do
    check all(a <- not_two_track()) do
      assert a == Rails.unwrap!({:ok, a})
    end
  end

  property "unwrap!/1 exception" do
    check all(e <- member_of([ArgumentError, RuntimeError, MatchError])) do
      assert_raise e, fn ->
        Rails.unwrap!({:error, struct(e, %{})})
      end
    end
  end

  property "unwrap!/1 other error" do
    check all(x <- not_two_track()) do
      assert x == catch_throw(Rails.unwrap!({:error, x}))
    end
  end

  property "unwrap!/1 not wrapped" do
    check all(x <- not_two_track()) do
      assert_raise FunctionClauseError, fn ->
        Rails.unwrap!(x)
      end
    end
  end

  property "mirror/1 flips result" do
    check all(tt <- two_track(term(), term())) do
      if match?({:ok, _}, tt) do
        assert {:error, _} = Rails.mirror(tt)
      else
        assert {:ok, _} = Rails.mirror(tt)
      end
    end
  end

  property "supervise/1 works" do
    check all(
            f <- one_track_fun(not_two_track()),
            g <- one_track_fun(not_two_track()),
            x <- two_track(not_two_track(), not_two_track())
          ) do
      assert_two_track(Rails.supervise(f, g).(x))
    end
  end

  property "log/1 logs stuff" do
    check all(x <- not_two_track()) do
      capture_log(fn ->
        assert {:ok, ^x} = Rails.log({:ok, x}, fn y -> "y: #{inspect(y)}" end)
      end)
    end
  end

  property "log/1 logs exception" do
    check all(e <- member_of([[%ArgumentError{}, %RuntimeError{}, %MatchError{}]])) do
      capture_log(fn ->
        assert {:error, ^e} = Rails.log({:error, e}, fn y -> "y: #{inspect(y)}" end)
      end)
    end
  end

  property "log/1 logs non-exception" do
    check all(e <- one_of([binary(), float(), map_of(binary(), binary())])) do
      capture_log(fn ->
        assert {:error, ^e} = Rails.log({:error, e}, fn y -> "y: #{inspect(y)}" end)
      end)
    end
  end

  property "error_message w/ exception" do
    check all(
            e <- member_of([ArgumentError, RuntimeError, ArithmeticError]),
            msg <- binary()
          ) do
      assert is_binary(Rails.error_message(struct(e, message: msg)))
    end
  end

  def id(x), do: x

  def two_track_id({:ok, _} = x), do: x
  def two_track_id({:error, _} = x), do: x

  def val(:one_track), do: not_two_track()
  def val(:switch), do: not_two_track()
  def val(:two_track), do: two_track(term(), term())

  def fun(:one_track, a, _), do: one_track_fun(a)
  def fun(:switch, a, b), do: switch_fun(a, b)
  def fun(:two_track, a, b), do: two_track_fun(a, b)

  def one_track_fun_raise do
    gen all(msg <- binary()) do
      fn _ ->
        raise %RuntimeError{message: msg}
      end
    end
  end

  def one_track_fun_throw do
    gen all(val <- term()) do
      fn _ ->
        throw(val)
      end
    end
  end

  def one_track_fun_exit do
    gen all(t <- term()) do
      fn _ ->
        exit(t)
      end
    end
  end

  def one_track_fun(b) do
    gen all(ret <- b) do
      fn
        {:ok, _} = x -> raise %ArgumentError{message: "bad arg: #{inspect(x)}"}
        {:error, _} = x -> raise %ArgumentError{message: "bad arg: #{inspect(x)}"}
        _ -> ret
      end
    end
  end

  def two_track(a, b) do
    one_of([
      tuple({constant(:ok), a}),
      tuple({constant(:error), b})
    ])
  end

  def switch_fun(c, d) do
    gen all(tt <- two_track(c, d)) do
      fn
        {:ok, _} = x -> raise %ArgumentError{message: "bad arg: #{inspect(x)}"}
        {:error, _} = x -> raise %ArgumentError{message: "bad arg: #{inspect(x)}"}
        _ -> tt
      end
    end
  end

  def two_track_fun(c, d) do
    gen all(tt <- two_track(c, d)) do
      fn
        {:ok, _} -> tt
        {:error, _} -> tt
        x -> raise %ArgumentError{message: "bad arg: #{inspect(x)}"}
      end
    end
  end

  def tee do
    constant(fn _ -> :ok end)
  end

  def two_track?(x),
    do: match?({:ok, _}, x) or match?({:error, _}, x)

  def assert_two_track(x) do
    assert two_track?(x)
  end

  def refute_two_track(x) do
    refute two_track?(x)
  end

  def not_two_track do
    filter(term(), &(not two_track?(&1)))
  end
end
