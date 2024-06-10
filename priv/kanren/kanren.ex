defimpl Kanren.Reifiable, for: Logic.LazyTree do
  alias Logic.Bindable

  def reify(%@for{next: nil, thunk: nil}, s), do: s
  def reify(%@for{next: nil, thunk: f}, s), do: Kanren.reify_s(f.(), s)
  def reify(%@for{next: {:just, a}, thunk: nil}, s), do: Kanren.reify_s(a, s)
  def reify(%@for{next: {:just, a}, thunk: f}, s),
   do: Kanren.reify_s(a, s) or @protocol.reify(f.(), s)

  def unify(u, %@for{} = v, s) do
    with {nu, tu} <- Bindable.next(u),
         {nv, tv} <- Bindable.next(v),
         s when not is_nil(s) <- Kanren.unify(nu, nv, s) do
      @protocol.unify(tu, tv, s)
    else
      _ -> nil
    end
  end
  def unify(_u, _v, _s), do: nil

  def occurs_check(%@for{next: nil, thunk: nil}, _v, _s), do: false
  def occurs_check(%@for{next: nil, thunk: f}, v, s), do: Kanren.occurs_check(f.(), v, s)
  def occurs_check(%@for{next: {:just, a}, thunk: nil}, v, s), do: Kanren.occurs_check(a, v, s)
  def occurs_check(%@for{next: {:just, a}, thunk: f}, v, s),
    do: Kanren.occurs_check(v, a, s) or @protocol.occurs_check(f.(), v, s)

  def walk(%@for{next: nil, thunk: nil}, _f), do: @for.empty()
  def walk(%@for{next: nil, thunk: t}, f), do: @protocol.walk(t.(), f)
  def walk(%@for{next: {:just, a}, thunk: nil}, f),
    do: a |> f.() |> @for.just() |> @for.constant()
  def walk(%@for{next: {:just, a}, thunk: t}, f),
    do: @for.new(@for.just(f.(a)), @for.thunk(fn -> @protocol.walk(t.(), f) end))

  def is_tree?(_), do: true
end

defmodule Kanren do
  @moduledoc """
  """
  use Boundary, top_level?: true, deps: [Logic], exports: []
  use Logic
  require Logger
  alias Kanren.LVar
  alias Kanren.Reifiable
  alias Kanren.Store

  # TODO: more features! -> Common Goals, Tabling, CLP(Tree), CLP(FD)
  #   defmodule Kanren.Goals
  #   defmodule Kanren.CLP.Tree
  #   defmodule Kanren.CLP.Tree.Goals
  #   defmodule Kanren.CLP.FD
  #   defmodule Kanren.Tabling

  @type goal(a) :: (a -> Logic.t(a))
  @type goal() :: goal(Store.t())
  @type t() :: Reifiable.t()

  defmacro __using__(_opts) do
    quote do
      use Logic.Bind
      require Kanren
      import Kanren, only: [eq: 2, fresh!: 2, conde: 1, run!: 2, run!: 3]
    end
  end

  # @spec run([LVar.t()], keyword(), [goal()]) :: [%{required(LVar.t()) => any()}]
  defmacro run!(bindings, opts, [do: block]) do
    var_names = Enum.map(bindings, &elem(&1, 0))
    quote do
      n = Keyword.get(unquote(opts), :n, :infinity)

      Kanren.fresh!(unquote(bindings), [do: unquote(block)])
      |> then(fn goal -> goal.(Kanren.Store.empty()) end)
      |> then(&(if is_integer(n) and n > 0 do
        Stream.take(&1, n)
      else
        &1
      end))
      |> Kanren.reify_all(Kanren.LVar.lvars(0, unquote(var_names)))
      |> Enum.to_list()
    end
  end
  defmacro run!(bindings, [do: block]) do
    quote do: Kanren.run!(unquote(bindings), [], [do: unquote(block)])
  end

  @spec eq(t(), t()) :: goal()
  def eq(u, v), do: &eq(u, v, &1)

  @spec eq(t(), t(), Store.t()) :: Logic.t(Store.t())
  def eq(u, v, %{subs: subs} = s) do
    case unify(u, v, s) do
      nil -> mzero()
      %{subs: ^subs, log: []} -> unit(s)
      s -> unit(s)
    end
  end

  # @spec fresh([LVar.t()], [goal()]) :: goal()
  defmacro fresh!(bindings, [do: block]) do
    var_names = Enum.map(bindings, &elem(&1, 0))
    count = Enum.count(bindings)
    gs =
      case block do
        {:__block__, _, goals} -> goals
        goal -> [goal]
      end

    if count > 0 do
      quote do
        fn %{counter: counter} = s ->
          fn unquote(bindings) ->
            Bind.bind_many!(%{s | counter: counter + unquote(count)}, unquote(gs))
          end.(Kanren.LVar.lvars(counter, unquote(var_names)))
        end
      end
    else
      quote do
        fn s ->
          Bind.bind_many!(s, unquote(gs))
        end
      end
    end
  end
  defmacro fresh!(bindings, goals) do
    quote do
      require Logger
      _ = Logger.warning(fn -> %{msg: :no_match!, bindings: unquote(bindings), goals: unquote(goals)} end)
      fn _ -> Logic.mzero() end
    end
  end

  # # @spec conde([[goal(), ...], ...]) :: goal()
  # defmacro conde([do: {:__block__, _, clauses}]) do
  #   gs = Enum.map(clauses, fn [c | cs] ->
  #     quote do: Bind.bind_many!(unquote(c).(s), unquote(cs))
  #   end)

  #   quote do: fn s -> thunk(fn -> Bind.interleave_many!(unquote(gs)) end) end
  # end
  # defmacro conde([do: clause]) do
  #   call = {:__block__, [], [clause]}
  #   quote do: MK.conde(do: unquote(call))
  # end

  # @spec conda([[goal()]]) :: goal()
  # def conda(_clauses) do
  # end

  # defmacro conda!([do: {:__block__, _, _clauses}]) do
  #   quote do
  #   end
  # end
  # defmacro conda!([do: _clause]) do
  #   quote do
  #   end
  # end

  # @spec condu([[goal()]]) :: goal()
  # def condu(_clauses) do
  # end

  # defmacro condu!([do: {:__block__, _, _clauses}]) do
  #   quote do
  #   end
  # end
  # defmacro condu!([do: _clause]) do
  #   quote do
  #   end
  # end

  # @spec project([LVar.t()], [goal()]) :: goal()
  # def project(_bindings, _goals) do
  # end

  # defmacro project!([], [do: _]), do: raise("")
  # defmacro project!(_bindings, [do: _block]) do
  #   quote do
  #   end
  # end

  @spec reify_all(Enumerable.t(Store.t()), [LVar.t()]) :: Enumerable.t(%{required(LVar.t()) => any()})
  def reify_all(ss, vs) do
    ss
    |> Stream.map(&tap(&1, fn s -> Logger.debug(fn -> %{f: :reify_all, s: s} end) end))
    |> Stream.map(&reify_vs(&1, vs))
  end

  @spec reify_vs(Store.t(), [LVar.t()]) :: %{required(LVar.t()) => any()}
  def reify_vs(s, vs) do
    vs
    |> Stream.map(&tap(&1, fn v -> Logger.debug(fn -> %{f: :reify_vs, v: v} end) end))
    |> Enum.reduce(%{}, &Map.put(&2, &1, reify(s, &1)))
  end

  @spec reify(Store.t(), LVar.t()) :: any()
  def reify(s, v) do
    _ = Logger.debug(fn -> %{f: :reify, p: {:pre, :walk_all}, s: s, v: v} end)
    v = walk_all(v, s)
    _ = Logger.debug(fn -> %{f: :reify, p: {:post, :walk_all}, s: s, v: v} end)
    case reify_s(v, Store.new()) do
      %{subs: ss} when map_size(ss) == 0 -> v
      r ->
        _ = Logger.debug(fn -> %{f: :reify, p: {:pre, :sub_walk_all}, s: s, v: v} end)
        walk_all(v, r)
        |> tap(&Logger.debug(fn -> %{f: :reify, p: {:pre, :sub_walk_all}, s: s, v: &1} end))
    end
  end

  @spec reify_s(t(), Store.t()) :: Store.t()
  def reify_s(v, s), do: Reifiable.reify(walk(v, s), s)

  @spec unify(t(), t(), Store.t()) :: Store.t() | nil
  def unify(u, v, s)
  def unify(u, u, s), do: s
  def unify(u, v, s) do
    {u, v} = {walk(u, s), walk(v, s)}
    case {u, LVar.lvar?(u), v, LVar.lvar?(v)} do
      {u, true, u, _} -> s
      {_, false, _, true} -> Reifiable.unify(v, u, s)
      _ -> Reifiable.unify(u, v, s)
    end
  end

  @spec occurs_check(t(), t(), Store.t()) :: Store.t()
  def occurs_check(u, v, s), do: Store.occurs_check(s, u, v)

  @spec walk_all(t(), Store.t()) :: t()
  def walk_all(v, s), do: Store.walk_all(s, v)

  @spec walk(t(), Store.t()) :: t()
  def walk(v, s), do: Store.walk(s, v)

  defmodule LVar do
    @moduledoc """
    """

    defstruct [:id, :name]
    @type t :: %__MODULE__{
      id: id(),
      name: name()
    }

    @type id :: integer()
    @type name :: atom()

    @doc """
    """
    @spec lvar(name(), id()) :: t()
    def lvar(name, id) do
      %__MODULE__{
        id: id,
        name: name
      }
    end

    @doc """
    """
    @spec lvars([{name(), id()}]) :: [t()]
    def lvars(data) do
      Enum.map(data, fn {name, id} -> lvar(name, id) end)
    end

    @doc """
    """
    @spec lvars(non_neg_integer(), [name()]) :: [t()]
    def lvars(starting_id, names) do
      starting_id..(starting_id + length(names) - 1)
      |> then(&Enum.zip(names, &1))
      |> lvars()
    end

    @doc """
    """
    @spec lvar?(any()) :: boolean()
    def lvar?(%__MODULE__{}), do: true
    def lvar?(_), do: false

    @doc false
    defmacro __using__(_opts) do
      quote do
        import Kanren.LVar
        alias Kanren.LVar
      end
    end

    defimpl Kanren.Reifiable do
      alias Kanren.Store

      def reify(v, s), do: Store.extend(s, v, reify_name(v))
      defp reify_name(v), do: v.name

      def unify(u, u, s), do: s
      def unify(u, v, s), do: Store.extend(s, u, v)

      def occurs_check(u, v, s), do: Kanren.walk(u, s) == v

      def walk(v, f), do: f.(v)

      def is_tree?(_), do: false
    end

    defimpl Inspect do
      def inspect(%{name: name, id: id}, _opts) do
        "lvar(#{inspect(name)}, #{id})"
      end
    end
  end

  defmodule Store do
    @moduledoc """
    """
    alias Kanren.LVar
    alias Kanren.Reifiable

    defstruct subs: %{},
              log: [],
              counter: 0,
              occurs_check?: true
    @type t :: %__MODULE__{
      subs: substitutions(),
      log: log(),
      counter: integer(),
      occurs_check?: boolean()
    }

    @type substitutions :: map()
    @type log :: [{any(), any()}]

    @doc """
    """
    @spec new() :: t()
    def new, do: %__MODULE__{}

    @doc """
    """
    @spec empty() :: t()
    def empty, do: new()

    @doc """
    """
    @spec extend(t(), Kanren.t(), Kanren.t(), keyword()) :: t() | nil
    def extend(s, u, v, opts \\ [])
    def extend(%__MODULE__{occurs_check?: true} = s, u, v, opts) do
      unless occurs_check(s, u, v) do
        extend_no_check(s, u, v, opts)
      end
    end
    def extend(s, u, v, opts),
      do: extend_no_check(s, u, v, opts)

    @doc """
    """
    @spec extend_no_check(t(), Kanren.t(), Kanren.t(), keyword()) :: t()
    def extend_no_check(s, u, v, opts \\ []) do
      s
      |> Map.update!(:subs, &Map.put(&1, u, v))
      |> then(&(if Keyword.get(opts, :log?, true) do
        Map.update!(&1, :log, fn xs -> [{u, v} | xs] end)
      else
        &1
      end))
    end

    @doc """
    """
    @spec occurs_check(t(), Kanren.t(), Kanren.t()) :: boolean()
    def occurs_check(s, u, v),
      do: Reifiable.occurs_check(u, walk(s, v), s)

    @doc """
    """
    @spec walk_all(t(), Kanren.t()) :: Kanren.t()
    def walk_all(s, v) do
      s
      |> walk(v)
      |> Reifiable.walk(&walk_all(s, &1))
    end

    @doc """
    """
    @spec walk(t(), Kanren.t()) :: Kanren.t()
    def walk(%{subs: subs} = s, v) do
      with true <- LVar.lvar?(v),
           {:ok, v} <- Map.fetch(subs, v) do
        walk(s, v)
      else
        _ -> v
      end
    end

    defimpl Logic.Bindable do
      alias Logic.LazyTree

      def next(s), do: {s, s}

      def interleave(a, b),
        do: LazyTree.new(a, fn -> LazyTree.constant(b) end)

      def bind(s, k), do: k.(s)
    end
  end
end
