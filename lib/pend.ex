defmodule Pend do
  @moduledoc """
  This is an 'approximation' of at Pause Monad, modelled after the Haskell
  implementation found here: https://stackoverflow.com/questions/10236953/the-pause-monad

  ```haskell
  data Pause s a = Pause { runPause :: s -> (PauseResult s a, s) }

  data PauseResult s a
      = Done a
      | Suspend (Pause s a)

  instance Monad (Pause s) where
      return a = Pause (\s -> (Done a, s))
      m >>= k = Pause $ \s ->
          case runPause m s of
              (Done a, s') -> runPause (k a) s'
              (Suspend m', s') -> (Suspend (m' >>= k), s')

  get :: Pause s s
  get = Pause (\s -> (Done s, s))

  put :: s -> Pause s ()
  put s = Pause (\_ -> (Done (), s))

  yield :: Pause s ()
  yield = Pause (\s -> (Suspend (return ()), s))

  step :: Pause s () -> s -> (Maybe (Pause s ()), s)
  step m s =
      case runPause m s of
          (Done _, s') -> (Nothing, s')
          (Suspend m', s') -> (Just m', s')
  ```

  ## Examples

  Let's start with a straight-forward example, nothing strange...

    iex> 42
    ...> |> return()
    ...> |> bind(fn a -> return(a + 3) end)
    ...> |> bind(fn a -> return(a / 10) end)
    ...> |> bind(fn a -> return(a - 1) end)
    ...> |> run([])
    {3.5, []}

  On the surface this seems like an unnecessarily complicated version of
  piping...but let's add something interesting!

    iex> {{:suspend, m}, 45 = s} = 42
    ...> |> return()
    ...> |> bind(fn a -> return(a + 3) end) |> bind(put()) |> bind(yield())
    ...> |> bind(fn a -> return(a / 10) end) |> bind(put()) |> bind(yield())
    ...> |> bind(fn a -> return(a - 1) end) |> bind(put()) |> bind(yield())
    ...> |> step([])
    iex> {{:suspend, m}, 4.5 = s} = step(m, s)
    iex> {{:suspend, m}, 3.5 = s} = step(m, s)
    iex> step(m, s)
    {{:done, 3.5}, 3.5}

  See how we can run part of the computation, stop...and resume it later?
  """
  use Boundary, top_level?: true, deps: [], exports: []

  @typedoc """
  A suspended computation continuation.

  Given a state `s`, will return a tuple containing a `t:result/2` and the
  new state.
  """
  @type pause(s, a) :: (s -> {result(s, a), s})

  @typedoc """
  A paused result: either the result itselt (i.e. `{:done, _}`) or a suspended
  result with a continuation. Running the continuation function will continue
  the suspended computation.
  """
  @type result(s, a) :: {:done, a} | {:suspend, pause(s, a)}

  @doc """
  This is similar to the Elixir pipe `|>/2` for suspendable
  computations.

  It connects a function of `(s -> {result(s, a), s})` to
  `(a -> (s -> {result(s, a), s}))`.

  ## Examples

    iex> return(42)
    ...> |> bind(return())
    ...> |> run([])
    {42, []}

    iex> return(42)
    ...> |> bind(fn a -> return(a + 1) end)
    ...> |> run([])
    {43, []}
  """
  @spec bind(pause(s, a), (a -> pause(s, b))) :: pause(s, b) when s: any, a: any, b: any
  def bind(m, k), do: &bind(&1, m, k)

  @doc false
  @spec bind(s, pause(s, a), (a -> pause(s, b))) :: {result(s, b), s} when s: any, a: any, b: any
  def bind(s, m, k) do
    case m.(s) do
      {{:done, a}, ss} ->
        k.(a).(ss)

      {{:suspend, mm}, ss} ->
        {{:suspend, bind(mm, k)}, ss}
    end
  end

  @doc """
  Similar to `return/1`, but easier to use as an identity function
  with `bind/2`.
  """
  @spec return() :: (a -> pause(s, a)) when s: any, a: any
  def return, do: &return/1

  @doc """
  Returns the given value, with no state changes.

  Similar to an identity function.

  ## Examples

    iex> return(:foo)
    ...> |> step([])
    {{:done, :foo}, []}
  """
  @spec return(a) :: pause(s, a) when s: any, a: any
  def return(a), do: fn s -> {{:done, a}, s} end

  @doc """
  Similar to `yield/1`, but easier to use as an identity function
  with `bind/2`.
  """
  @spec yield() :: (a -> pause(s, a)) when s: any, a: any
  def yield, do: &yield/1

  @doc """
  Returns the given value, with no state changes.

  Similar to an identity function.

  ## Examples

    iex> {{:suspend, m}, [] = s} = :bar
    ...> |> yield()
    ...> |> step([])
    iex> step(m, s)
    {{:done, :bar}, []}
  """
  @spec yield(a | pause(s, a)) :: pause(s, a) when s: any, a: any
  def yield(a)
  def yield(m) when is_function(m, 1), do: fn s -> {{:suspend, m}, s} end
  def yield(a), do: fn s -> {{:suspend, return(a)}, s} end

  @doc """
  Take the state and replace the return value with it.

  ## Examples

    iex> :foo
    ...> |> return()
    ...> |> bind(get())
    ...> |> run(:bar)
    {:bar, :bar}
  """
  @spec get() :: (a -> pause(s, a)) when s: any, a: any
  def get, do: get(fn _, s -> s end)

  @doc """
  Return state with a lens.

  ## Examples

    iex> %{a: :foo}
    ...> |> return()
    ...> |> bind(get(&Map.merge(&1, &2)))
    ...> |> run(%{b: :bar})
    {%{a: :foo, b: :bar}, %{b: :bar}}
  """
  @spec get((a, s -> a)) :: (a -> pause(s, a)) when s: any, a: any
  def get(f), do: &get(&1, f)

  @doc false
  @spec get(a, (a, s -> a)) :: pause(s, a) when s: any, a: any
  def get(a, f), do: fn s -> {{:done, f.(a, s)}, s} end

  @doc """
  Update state with a lens.

  ## Examples

    iex> %{a: :foo}
    ...> |> return()
    ...> |> bind(update(&Map.merge(&1, &2)))
    ...> |> run(%{b: :bar})
    {%{a: :foo}, %{a: :foo, b: :bar}}
  """
  @spec update((a, s -> s)) :: (a -> pause(s, a)) when s: any, a: any
  def update(f), do: &update(&1, f)

  @doc false
  @spec update(a, (a, s -> s)) :: pause(s, a) when s: any, a: any
  def update(a, f), do: fn s -> {{:done, a}, f.(a, s)} end

  @doc """
  Modify the return and state combined.

  ## Examples

    iex> %{a: :foo}
    ...> |> return()
    ...> |> bind(get_and_update(fn a, s -> {s, a} end))
    ...> |> run(%{b: :bar})
    {%{b: :bar}, %{a: :foo}}
  """
  @spec get_and_update((a, s -> {a, s})) :: (a -> pause(s, a)) when s: any, a: any
  def get_and_update(f), do: &get_and_update(&1, f)

  @doc false
  @spec get_and_update(a, (a, s -> {a, s})) :: pause(s, a) when s: any, a: any
  def get_and_update(a, f) do
    fn s ->
      {a, s} = f.(a, s)
      {{:done, a}, s}
    end
  end

  @doc """
  Replace the state with the return value.

  ## Examples

    iex> :foo
    ...> |> return()
    ...> |> bind(put())
    ...> |> bind(fn _ -> return(:bar) end)
    ...> |> run([])
    {:bar, :foo}
  """
  @spec put() :: (a -> pause(a, a)) when a: any
  def put, do: &put(&1, &1)

  @doc """
  Replace the current state with `s`.

  ## Examples

    iex> :foo
    ...> |> return()
    ...> |> bind(put(:bar))
    ...> |> run([])
    {:foo, :bar}
  """
  @spec put(s) :: (a -> pause(s, a)) when s: any, a: any
  def put(s), do: &put(&1, s)

  @doc false
  @spec put(a, s) :: pause(s, a) when s: any, a: any
  def put(a, s), do: update(a, fn _, _ -> s end)

  @doc """
  Execute a single step.
  """
  @spec step(pause(s, a), s) :: {result(s, a), s} when s: any, a: any
  def step(m, s), do: m.(s)

  @doc """
  Keep running steps until done.
  """
  @spec run(pause(s, a), s) :: {a, s} when s: any, a: any
  def run(m, s) do
    case step(m, s) do
      {{:done, a}, s} -> {a, s}
      {{:suspend, m}, s} -> run(m, s)
    end
  end
end
