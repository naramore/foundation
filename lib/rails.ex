defmodule Rails do
  @moduledoc """
  Partial Railway Oriented Programming implementation in Elixir.

  See this [blog article](https://fsharpforfunandprofit.com/rop/) and this
  [other blog article](https://fsharpforfunandprofit.com/posts/recipe-part2/)
  for more details.

  ## Examples

  Lets use some of the adapters provided to pipe some functions together...

    iex> 15
    ...> |> switch(fn x -> x + 2 end)
    ...> |> bind(fn x -> {:ok, x * 2} end)
    ...> |> map(fn x -> x - 10 end)
    ...> |> then(fn
    ...>   {:ok, x} -> {:ok, x + 9000}
    ...>   {:error, x} -> {:error, x / 100}
    ...> end)
    {:ok, 9024}

  Now lets see what happens if an error occurs in the middle...

    iex> 15
    ...> |> switch(fn x -> x + 2 end)
    ...> |> bind(fn x -> {:error, x} end)
    ...> |> bind(fn x -> {:ok, x * 2} end)
    ...> |> map(fn x -> x - 10 end)
    ...> |> then(fn
    ...>   {:ok, x} -> {:ok, x + 9000}
    ...>   {:error, x} -> {:error, x / 100}
    ...> end)
    {:error, 0.17}
  """

  require Logger

  @type success(x) :: {:ok, x}
  @type success :: success(any)
  @type failure(reason) :: {:error, reason}
  @type failure :: failure(any)
  @type result(x, reason) :: success(x) | failure(reason)
  @type result :: result(any, any)

  @typedoc """
  This is just a function of arity 1.

  The reasoning for the terminology 'one-track' is in contrast to
  `t:two_track_fun/4`. See this article on
  [Railway Oriented Programming](https://fsharpforfunandprofit.com/posts/recipe-part2/#railway-oriented-programming)
  for more details.
  """
  @type one_track_fun(a, b) :: (a -> b)

  @type two_track(x, reason) :: result(x, reason)
  @type two_track(x) :: two_track(x, any)

  @typedoc """
  This is a function that maps 'normal' `t:one_track_fun/2` to
  `t:two_track_fun/4`.

  It takes a normal non-`t:two_track/2` input and outputs `t:two_track/2`.
  """
  @type switch_fun(a, b, c) :: (a -> two_track(b, c))
  @type switch_fun(a, b) :: switch_fun(a, b, any)

  @typedoc """
  This is the core building block of Railway Oriented Programming.

  A function that takes a `t:two_track/2` as input and returns a `two_track/2`
  as output. These can be piped together indefinately.
  """
  @type two_track_fun(a, b, c, d) :: (two_track(a, c) -> two_track(b, d))
  @type two_track_fun(a, b) :: two_track_fun(a, b, any, any)

  @type chardata_or_fun :: Logger.message() | (-> Logger.message())

  @doc """
  Returns an `:ok` tuple.

  ## Examples

    iex> success(42)
    {:ok, 42}
  """
  @spec success(a) :: success(a) when a: any
  def success(a), do: {:ok, a}

  @doc """
  Returns an `:error` tuple.

  ## Examples

    iex> failure(:fake_error)
    {:error, :fake_error}
  """
  @spec failure(reason) :: failure(reason) when reason: any
  def failure(reason), do: {:error, reason}

  @doc """
  Ensures the given `x` is wraped as a `t:result/2`.

  ## Examples

    iex> wrap({:ok, :foo})
    {:ok, :foo}
    iex> wrap({:error, :bar})
    {:error, :bar}
    iex> wrap(:baz)
    {:ok, :baz}
  """
  @spec wrap(any) :: result()
  def wrap(x)
  def wrap({:ok, _} = x), do: x
  def wrap({:error, _} = x), do: x
  def wrap(x), do: success(x)

  @doc """
  Unwraps a `t:result/2` `x` into a value.

  If an error, will either raise the exception or throw (the not-exception).

  ## Examples

    iex> unwrap!({:ok, 42})
    42
    iex> unwrap!({:error, %RuntimeError{message: "some error"}})
    ** (RuntimeError) some error
  """
  @spec unwrap!(result(x, reason)) :: x | no_return when x: any, reason: any
  def unwrap!(x)

  def unwrap!({:ok, x}), do: x

  def unwrap!({:error, %{__exception__: true} = error}) do
    raise error
  end

  def unwrap!({:error, reason}) do
    throw(reason)
  end

  @doc """
  Flips a `t:success/1` into `t:failure/1` and vice-versa.

  ## Examples

    iex> mirror({:ok, :foo})
    {:error, :foo}
    iex> mirror({:error, :bar})
    {:ok, :bar}
  """
  @spec mirror(result(a, b)) :: result(b, a) when a: any, b: any
  def mirror({:ok, x}), do: failure(x)
  def mirror({:error, r}), do: success(r)

  @doc """
  Adapts a normal `t:one_track_fun/2` `f` into a `t:switch_fun/2`.

  ## Examples

    iex> sf = switch(fn x -> x + 1 end)
    iex> sf.(10)
    {:ok, 11}
  """
  @spec switch((a -> b)) :: switch_fun(a, b) when a: any, b: any
  def switch(f), do: &switch(&1, f)

  @doc """
  Similar to `switch/1` but easier to use in a pipe.
  """
  @spec switch(a, (a -> b)) :: two_track(a, b) when a: any, b: any
  def switch(a, f), do: success(f.(a))

  @doc """
  Adapts a `t:switch_fun/2` `f` into a `t:two_track_fun/4`.

  ## Examples

    iex> ttf = bind(fn x -> {:ok, x * 10} end)
    iex> ttf.({:ok, 3})
    {:ok, 30}
    iex> ttf.({:error, :foo})
    {:error, :foo}

    iex> ttf = bind(fn x -> {:error, x / 3} end)
    iex> ttf.({:ok, 96})
    {:error, 32.0}
    iex> ttf.({:error, 144})
    {:error, 144}
  """
  @spec bind(switch_fun(a, b)) :: two_track_fun(a, b) when a: any, b: any
  def bind(f), do: &bind(&1, f)

  @doc """
  Similar to `bind/1` but easier to use in a pipe.
  """
  @spec bind(two_track(a), switch_fun(a, b)) :: two_track(b) when a: any, b: any
  def bind({:error, reason}, _), do: {:error, reason}
  def bind({:ok, a}, f), do: f.(a)

  @doc """
  Adapts a normal `t:one_track_fun/2` `f` into a `t:two_track_fun/4`.

  ## Examples

    iex> ttf = map(fn x -> x * 10 end)
    iex> ttf.({:ok, 3})
    {:ok, 30}
    iex> ttf.({:error, :foo})
    {:error, :foo}
  """
  @spec map((a -> b)) :: two_track_fun(a, x, b, x) when a: any, b: any, x: any
  def map(f), do: &map(&1, f)

  @doc """
  Similar to `map/1` but easier to use in a pipe.
  """
  @spec map(two_track(a), (a -> b)) :: two_track(b) when a: any, b: any
  def map({:error, reason}, _), do: failure(reason)
  def map({:ok, a}, f), do: success(f.(a))

  @doc """
  Same as `map/1`, but adapts the normal `t:one_track_fun/2` `f` to the failure
  path, instead of the success path (as in `map/1`).
  """
  @spec fmap((a -> b)) :: two_track_fun(x, a, x, b) when a: any, b: any, x: any
  def fmap(f), do: &fmap(&1, f)

  @doc """
  Similar to `fmap/1` but easier to use in a pipe.
  """
  @spec fmap(two_track(x, a), (a -> b)) :: two_track(x, b) when a: any, b: any, x: any
  def fmap(x, f), do: x |> mirror() |> map(f) |> mirror()

  @doc """
  Adapts a side-effectful function (whose result doesn't matter) to a `t:one_track_fun/2`.

  ## Examples

    iex> tf = tee(fn _ -> :ok end)
    iex> tf.(42)
    42
  """
  @spec tee((a -> :ok)) :: (a -> a) when a: any
  def tee(f), do: &tee(&1, f)

  @doc """
  Similar to `tee/1` but easier to use in a pipe.
  """
  @spec tee(a, (a -> :ok)) :: a when a: any
  def tee(a, f) do
    _ = f.(a)
    a
  end

  @doc """
  Similar to `switch/1`, but rescues / catches and converts those errors into
  `t:failure/1`.

  ## Examples

    iex> sf = safe(fn _ -> raise %RuntimeError{} end)
    iex> sf.(nil)
    {:error, %RuntimeError{}}

    iex> sf = safe(fn _ -> throw :bar end)
    iex> sf.(nil)
    {:error, {:caught, :bar}}

    iex> sf = safe(fn _ -> exit :foo end)
    iex> sf.(nil)
    {:error, {:exit, :foo}}
  """
  @spec safe((a -> b | no_return)) :: switch_fun(a, b) when a: any, b: any
  def safe(f), do: &safe(&1, f)

  @doc """
  Similar to `safe/1` but easier to use in a pipe.
  """
  @spec safe(a, (a -> b | no_return)) :: two_track(b) when a: any, b: any
  def safe(a, f) do
    switch(f).(a)
  rescue
    reason -> failure(reason)
  catch
    :exit, reason -> failure({:exit, reason})
    x -> failure({:caught, x})
  end

  @doc """
  Creates a `t:two_track_fun/4` from 2 `t:one_track_fun/2`.

    - given `{:ok, x}`, will excute `on_success.(x)`
    - given `{:error, r}`, will execute `on_fail.(r)`

  ## Examples

    iex> ttf = supervise(
    ...>    fn x -> x + 42 end,
    ...>    fn r -> r * 42 end
    ...> )
    iex> ttf.({:ok, 7})
    {:ok, 49}
    iex> ttf.({:error, 3})
    {:error, 126}
  """
  @spec supervise((a -> b), (c -> d)) :: two_track_fun(a, b, c, d)
        when a: any, b: any, c: any, d: any
  def supervise(on_success, on_fail), do: &supervise(&1, on_success, on_fail)

  @doc """
  Similar to `supervise/2` but easier to use in a pipe.
  """
  @spec supervise(two_track(a, c), (a -> b), (c -> d)) :: two_track(b, d)
        when a: any, b: any, c: any, d: any
  def supervise({:error, reason}, _on_success, on_fail), do: failure(on_fail.(reason))
  def supervise({:ok, a}, on_success, _on_fail), do: success(on_success.(a))

  @doc """
  Logs `t:two_track/2` in a pipeline.

  Will `Logger.info/2` when the result is `{:ok, _}` and will `Logger.error/2`
  when the result is `{:error, _}`.
  """
  @spec log(two_track(a), (a -> chardata_or_fun), keyword) :: two_track(a) when a: any
  def log(two_track, message_fun, metadata \\ []) do
    supervise(
      two_track,
      tee(&Logger.info(message_fun.(&1), metadata)),
      tee(&Logger.error(fn -> error_message(&1) end, metadata))
    )
  end

  @doc false
  @spec error_message(reason :: any) :: Logger.message()
  def error_message(%{__exception__: true} = error) do
    Exception.format(:error, error, [])
  end

  def error_message(reason) do
    case String.Chars.impl_for(reason) do
      nil -> inspect(reason)
      otherwise -> to_string(otherwise)
    end
  end

  # coveralls-ignore-start

  @doc """
  Similar to `plus/4` but clones the input to both paths.

  ## Examples

    iex> sf = split(
    ...>    map(fn x -> x + 2 end),
    ...>    map(fn x -> x * 2 end),
    ...>    fn x, y -> {x, y} end,
    ...>    fn x, y -> [x, y] end
    ...> )
    iex> sf.({:ok, 10})
    {:ok, {12, 20}}
    iex> sf.({:error, :foo})
    {:error, [:foo, :foo]}
  """
  @spec split(two_track_fun(a, b, c, d), two_track_fun(a, b, y, z), (c, y -> i), (d, z -> j)) ::
          two_track_fun(i, d | z | j)
        when a: any, b: any, c: any, d: any, y: any, z: any, i: any, j: any
  def split(f, g, add_ok, add_error),
    do: &split(&1, f, g, add_ok, add_error)

  @doc """
  Similar to `split/4` but easier to use in a pipe.
  """
  @spec split(
          two_track(a, b),
          two_track_fun(a, b, c, d),
          two_track_fun(a, b, y, z),
          (c, y -> i),
          (d, z -> j)
        ) :: two_track(i, d | z | j)
        when a: any, b: any, c: any, d: any, y: any, z: any, i: any, j: any
  def split({p, x}, f, g, add_ok, add_error) do
    plus({p, {x, x}}, f, g, add_ok, add_error)
  end

  @doc """
  Composes two `two_track_fun/4` `f` and `g` in "parallel".

  ## Examples

    iex> pf = plus(
    ...>    map(fn x -> x + 2 end),
    ...>    map(fn x -> x * 2 end),
    ...>    fn x, y -> {x, y} end,
    ...>    fn x, y -> [x, y] end
    ...> )
    iex> pf.({:ok, {41, 42}})
    {:ok, {43, 84}}
    iex> pf.({:error, {:foo, :bar}})
    {:error, [:foo, :bar]}

  ## Notes

  This is NOT using `Task` or `spawn/3` for true concurrency.
  """
  @spec plus(
          two_track({a, w}, {b, x}),
          two_track_fun(a, b, c, d),
          two_track_fun(w, x, y, z),
          (c, y -> i),
          (d, z -> j)
        ) :: two_track_fun(i, d | z | j)
        when a: any, b: any, c: any, d: any, w: any, x: any, y: any, z: any, i: any, j: any
  def plus(f, g, add_ok, add_error),
    do: &plus(&1, f, g, add_ok, add_error)

  @doc """
  Similar to `plus/4` but easier to use in a pipe.
  """
  @spec plus(
          two_track({a, w}, {b, x}),
          two_track_fun(a, b, c, d),
          two_track_fun(w, x, y, z),
          (c, y -> i),
          (d, z -> j)
        ) :: two_track(i, d | z | j)
        when a: any, b: any, c: any, d: any, w: any, x: any, y: any, z: any, i: any, j: any
  def plus({p, {x, y}}, f, g, add_ok, add_error) when p in [:ok, :error] do
    case {f.({p, x}), g.({p, y})} do
      {{:ok, fx}, {:ok, gx}} -> success(add_ok.(fx, gx))
      {{:ok, _fx}, {:error, gr}} -> failure(gr)
      {{:error, fr}, {:ok, _gx}} -> failure(fr)
      {{:error, fr}, {:error, gr}} -> failure(add_error.(fr, gr))
    end
  end

  # coveralls-ignore-stop
end
