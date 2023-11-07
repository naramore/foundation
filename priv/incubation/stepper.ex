defmodule Stepper do
  @moduledoc """
  """

  # TODO: integrate w/ Step
  # TODO: integrate w/ Step.Reductions
  # TODO: docs
  # TODO: tests

  defstruct s: nil,
            a: nil,
            context: %{}
  @type t(s, a) :: %__MODULE__{
    s: s,
    a: a,
    context: map()
  }
  @type t :: t(any, any)
  @type id :: term()
  @type entry(s, a) :: %{
    id: id(),
    step: non_neg_integer(),
    run: non_neg_integer(),
    trigger:   a,
    old_model: s,
    new_model: s,
    result:    a
  }

  # {a, s} = Stepper.test()
  def test do
    f = fn i -> fn x -> IO.puts("[#{inspect(i)}] doing #{inspect(x)}..."); x + 1 end end

    m =
      42
      |> start()
      |> map(:a, f.(:a))
      |> map(:b, f.(:b))
      |> map(:c, f.(:c))
      |> map(:d, f.(:d))
      |> map(:e, f.(:e))

    s = nil
    {{:suspend, m}, s} = Stepper.step(m, s)
    {{:suspend, m}, s} = Stepper.step(m, s)
    {{:suspend, m}, s} = Stepper.step(m, s)
    {{:suspend, m}, s} = Stepper.step(m, s)
    {{:suspend, m}, s} = Stepper.step(m, s)
    {{:done, a}, %{a: a} = s} = Stepper.step(m, s)
    {a, s}
  end

  defdelegate return(), to: Pend
  defdelegate return(a), to: Pend
  defdelegate yield(), to: Pend
  defdelegate yield(a), to: Pend

  @doc """
  """
  @spec new(t(s, a) | {s, a}, map()) :: t(s, a) when s: any, a: any
  def new(s, context \\ %{})
  def new({s, a}, context) do
    new(%__MODULE__{s: s, a: a}, context)
  end
  def new(stepper, context) do
    Map.update!(stepper, :context, &Map.merge(&1, context))
  end

  @doc """
  """
  @spec step(Pend.pause(s, a), s) :: {Pend.result(t(s, a), a), t(s, a)} when s: any, a: any
  def step(m, s)
  def step(m, %__MODULE__{} = s), do: Pend.step(m, s)
  def step(m, s), do: step(m, new({s, nil}))

  @doc """
  """
  @spec run(Pend.pause(s, a), s, keyword()) :: {Pend.result(t(s, a), a), t(s, a)} when s: any, a: any
  def run(m, s, opts \\ [])
  def run(m, %__MODULE__{} = s, opts) do
    case step(m, s) do
      {{:done, a}, s} -> {{:done, a}, s}
      {{:suspend, m}, s} ->
        # TODO: implement breakpoints
        #       when suspended ->
        #         match on a / t(s, a)
        #         if matched -> return suspend
        #         otherwise -> run/3
        # TODO: implement remote agent that suspension control
        #       i.e. ^^^ + match all + check agent if should stop
        run(m, s, opts)
    end
  end
  def run(m, s, opts), do: run(m, new({s, nil}), opts)

  @doc """
  """
  @spec start(t(s, a)) :: Pend.pause(t(s, a), a) when s: any, a: any
  def start(a) do
    a
    |> return()
    |> Pend.bind(Pend.update(&post_start/2))
  end

  @doc """
  """
  @spec bind(Pend.pause(t(s, a), a), id(), (a -> Pend.pause(s, b))) :: Pend.pause(t(s, b), b) when s: any, a: any, b: any
  def bind(m, id \\ nil, k) do
    m
    |> Pend.bind(Pend.update(&pre_bind(&1, &2, id)))
    |> Pend.bind(k)
    |> Pend.bind(Pend.update(&post_bind/2))
    |> Pend.bind(yield())
  end

  @doc """
  """
  @spec map(Pend.pause(t(s, a), a), id(), (a -> b)) :: Pend.pause(t(s, b), b) when s: any, a: any, b: any
  def map(m, id \\ nil, f), do: bind(m, id, &return(f.(&1)))

  @doc """
  """
  @spec get() :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def get, do: get(fn _, s -> s end)

  @doc """
  """
  @spec get((a, s -> a)) :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def get(f), do: &get(&1, f)

  @doc false
  @spec get(a, (a, s -> a)) :: Pend.pause(t(s, a), a) when s: any, a: any
  def get(a, f) do
    Pend.get_and_update(a, get_lens(f))
  end

  @doc """
  """
  @spec update((a, s -> s)) :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def update(f), do: &update(&1, f)

  @doc false
  @spec update(a, (a, s -> s)) :: Pend.pause(t(s, a), a) when s: any, a: any
  def update(a, f) do
    Pend.update(a, update_lens(f))
  end

  @doc """
  """
  @spec get_and_update((a, s -> {a, s})) :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def get_and_update(f), do: &get_and_update(&1, f)

  @doc false
  @spec get_and_update(a, (a, s -> {a, s})) :: Pend.pause(t(s, a), a) when s: any, a: any
  def get_and_update(a, f) do
    Pend.get_and_update(a, get_and_update_lens(f))
  end

  @doc """
  """
  @spec put() :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def put, do: &put(&1, &1)

  @doc """
  """
  @spec put(s) :: (a -> Pend.pause(t(s, a), a)) when s: any, a: any
  def put(s), do: &put(&1, s)

  @doc false
  @spec put(a, s) :: Pend.pause(t(s, a), a) when s: any, a: any
  def put(a, s), do: update(a, fn _, _ -> s end)

  @spec get_lens((a, s -> a)) :: (a, t(s, a) -> {a, t(s, a)}) when s: any, a: any
  defp get_lens(f) do
    fn a, s ->
      aa = f.(a, s.s)
      {aa, %{s | a: aa}}
    end
  end

  @spec update_lens((a, s -> s)) :: (a, t(s, a) -> t(s, a)) when s: any, a: any
  defp update_lens(f),
    do: fn a, s -> %{s | s: f.(a, s.s)} end

  @spec get_and_update_lens((a, s -> {a, s})) :: (a, t(s, a) -> {a, t(s, a)}) when s: any, a: any
  defp get_and_update_lens(f) do
    fn a, s ->
      {aa, ss} = f.(a, s.s)
      {aa, %{s | s: ss, a: aa}}
    end
  end

  @spec post_start(a, t(s, a)) :: s when s: any, a: any
  defp post_start(_a, s) do
    Map.update!(s, :context, fn ctx ->
      ctx
      |> Map.put(:current_id, nil)
      |> Map.update(:cursor, {0, 0}, fn {_, r} -> {0, r + 1} end)
    end)
  end

  @spec pre_bind(a, t(s, a), id()) :: s when s: any, a: any
  defp pre_bind(a, s, id) do
    Map.update!(s, :context, fn ctx ->
      ctx
      |> Map.put(:current_id, id)
      |> Map.update(:history, [pre_audit(a, s.s)], &[pre_audit(a, s.s) | &1])
    end)
  end

  @spec post_bind(a, t(s, a)) :: s when s: any, a: any
  defp post_bind(a, s) do
    s
    |> Map.update!(:context, fn ctx ->
      ctx
      |> update_history(s, a)
      |> update_cursor_step()
    end)
    |> Map.put(:a, a)
  end

  @spec update_history(map(), t(s, a), a) :: map() when s: any, a: any
  defp update_history(context, s, a) do
    Map.update!(context, :history, fn [e | es] ->
      [post_audit(e, s, a) | es]
    end)
  end

  @spec update_cursor_step(map()) :: map()
  defp update_cursor_step(context) do
    Map.update(context, :cursor, {0, 0}, fn {s, r} -> {s + 1, r} end)
  end

  @spec pre_audit(a, s) :: entry(s, a) when s: any, a: any
  defp pre_audit(trigger, old_model) do
    %{
      id: nil,
      step: nil,
      run: nil,
      trigger:   trigger,
      old_model: old_model,
      new_model: nil,
      result:    nil
    }
  end

  @spec post_audit(entry(s, a), t(s, a), a) :: entry(s, a) when s: any, a: any
  defp post_audit(%{trigger: t, old_model: om}, %{s: new_model, context: %{current_id: id, cursor: {s, r}}}, result) do
    %{
      id: id,
      step: s,
      run: r,
      trigger:   t,
      old_model: om,
      new_model: new_model,
      result:    result
    }
  end
end
