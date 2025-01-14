defmodule Kanren do
  @moduledoc """
  [MiniKanren](http://minikanren.org/)

  ## Implementations
  - [Kanren in 18 Lines of Haskell](https://stackoverflow.com/questions/10843563/conda-condi-conde-condu#10848902)
  - [Relational Programming in miniKanren: Techniques, Applications, and Implementations](https://scholarworks.iu.edu/iuswrrest/api/core/bitstreams/27f1ebb8-5114-4fa5-b598-dcfaddfd6af5/content)
    - [Clojure core.logic](https://github.com/clojure/core.logic/blob/master/src/main/clojure/clojure/core/logic.clj)
  - [μKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
    - [µKanren](https://github.com/jasonhemann/microKanren/blob/master/microKanren.scm)
  - [The Reasoned Schemer miniKanren](https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-impl.scm)
  - [cKanren: miniKanren with Constraints](https://www.schemeworkshop.org/2011/papers/Alvis2011.pdf)
    - [cKanren](https://github.com/calvis/cKanren/tree/master)
  - [αKanren: A Fresh Name in Nominal Logic Programming](http://webyrd.net/alphamk/alphamk.pdf)
    - [alphaKanren](https://github.com/webyrd/alphaKanren/blob/master/alphaKanren.scm)
  - [rKanren: Guided Search in miniKanren](http://webyrd.net/scheme-2013/papers/Swords2013.pdf)
    - [rkanren](https://github.com/cgswords/rkanren/blob/master/condr-mk.scm)
  - [Backtracking, Interleaving, and Terminating Monad Transformers](https://okmij.org/ftp/papers/LogicT.pdf)
    - [logict: A backtracking logic-programming monad.](https://hackage.haskell.org/package/logict)
  - [miniKanren in Haskell](https://github.com/jvranish/MiniKanrenT)

  ## References
  - [A Framework for Extending microKanren with Constraints](http://andykeep.com/SchemeWorkshop2015/papers/sfpw3-2015-hemann-friedman.pdf)
  - [A Core.logic Primer](https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer)
  - [Introduction to Logic Programming with Clojure](https://github.com/frenchy64/Logic-Starter/wiki)
  - [CONS Should not CONS its Arguments, or, a Lazy Alloc is a Smart Alloc](https://www.cs.tufts.edu/~nr/cs257/archive/henry-baker/cons-lazy-alloc.pdf)
  - [CONS Should Not CONS Its Arguments, Part II](https://guenchi.github.io/Scheme/doc/CONS%20Should%20Not%20CONS%20Its%20Arguments%20Part%20II%20Cheney%20on%20the%20MTA.pdf)
  """
  use Boundary, deps: [], exports: []
end

# Kanren.Stream
#   @callback mzero() :: t(a) when a: any()
#   @callback return(a) :: t(a) when a: any()
#   @callback mplus(t(a), t(a)) :: t(a) when a: any()
#   @callback bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
#   @callback msplit(t(a)) :: {a, t(a)} | nil when a: any()
# Kanren.Substitutions
#   @callback lvar?(term()) :: boolean()
#   @callback occurs?(t(), lterm(), lterm()) :: boolean()
#   @callback extend_subs(t(), lterm(), lterm()) :: t()
#   @callback lookup_sub(t(), lvar()) :: {:ok, lterm()} | :error
#   @callback fresh_lvar(t(), atom()) :: {lvar(), t()}


# NOTE: can we do (reify|unify|occur-check)-term like walk-term?
#       (i.e. passing a function to it? does this even make sense?)

# TODO:  walk(logic_term(), State.t()) :: State.t()
# TODO: reify(logic_term(), State.t()) :: State.t()
# TODO: unify(logic_term(), logic_term(), State.t()) :: State.t() | nil

# Kanren.Substitutions.{Micro}
# Kanren.Stream.{List, Choice}

# Kanren
# Kanren.Micro
#   @callback eq(lterm(), lterm()) :: goal()
#   @callback exist((a -> goal())) :: goal() when a: any()
#   @callback conj(goal(), goal()) :: goal()
#   @callback disj(goal(), goal()) :: goal()
# Kanren.Mini
#   @callback ifa()
#   @callback ifu()
#   @spec conj_many([goal()]) :: goal()
#   @spec disj_many([goal()]) :: goal()
#   @spec exist((... -> goal())) :: goal()
#   @spec conde([[goal()]]) :: goal()
#   @spec run(keyword(), (... -> goal())) :: [term()]
#   @spec conda([[goal()]]) :: goal()
#   @spec condu([[goal()]]) :: goal()
#   @spec project((... -> goal())) :: goal()
# Kanren.Mini.Macros
#   defmacro snooze(f)
#   defmacro conj_many(goals)
#   defmacro disj_many(goals)
#   defmacro exist(bindings, [do: {:__block__, _, goals}])
#   defmacro conde([do: {:__block__, _, clauses}])
#   defmacro run(bindings, opts \\ [], [do: {:__block__, _, goals}])
#   defmacro conda([do: {:__block__, _, clauses}])
#   defmacro condu([do: {:__block__, _, clauses}])
#   defmacro project(bindings, [do: {:__block__, _, goals}])
# Kanren.Mini.Goals
#   @spec succeed() :: goal()
#   @spec fail() :: goal()
#   @spec heado([lterm(), ...], lterm()) :: goal()
#   @spec tailo([lterm(), ...], [lterm()]) :: goal()
#   @spec conso(lterm(), lterm(), [lterm(), ...]) :: goal()
#   @spec membero(lterm(), [lterm(), ...]) :: goal()
#   @spec appendo([lterm()], [lterm()], lterm()) :: goal()
#   @spec emptyo(lterm()) :: goal()
#   @spec onceo(goal()) :: goal()
#   @spec copy_term(lvar(), lvar()) :: goal()
#   @spec is(lterm(), lterm(), (term() -> term())) :: goal()
#   @spec fresho(lterm()) :: goal()
#   @spec staleo(lterm()) :: goal()
# Kanren.CLP.Tree
# Kanren.CLP.FD
# Kanren.Nominal
#   @callback fresh((a -> goal())) :: goal() when a: any()
#   @callback hash()
#   @callback tie()
# Kanren.Ranked
# Kanren.Tabled
# Kanren.Prob

defmodule Kanren.Core do
  alias Kanren.Stream
  alias Kanren.Substitutions

  @type s() :: Substitutions.t()
  @type goal(a) :: (a -> Stream.t(a))
  @type goal() :: goal(s())
  @type lterm() :: Kanren.Substitutions.lterm()

  defmacro __using__(opts) do
    stream = opts[:stream] || raise_missing_option(:stream)
    substitutions = opts[:substitutions] || raise_missing_option(:substitutions)
    quote do
      @stream unquote(stream)
      @substitutions unquote(substitutions)
    end
  end

  @spec raise_missing_option(atom()) :: no_return()
  defp raise_missing_option(arg) do
    raise %ArgumentError{message: "missing required #{arg} specification"}
  end
end

defmodule Kanren.Core.Micro do
  alias Kanren.Core

  @type goal() :: Core.goal()
  @type lterm() :: Core.lterm()

  @callback eq(lterm(), lterm()) :: goal()
  @callback exist((a -> goal())) :: goal() when a: any()
  @callback conj(goal(), goal()) :: goal()
  @callback disj(goal(), goal()) :: goal()

  defmacro __using__(_opts) do
    quote do
      @behaviour Kanren.Core.Micro
    end
  end
end

defmodule Kanren.Core.Mini do
  # @callback ifa
  # @callback ifu

  defmacro __using__(_opts) do
    quote do
      use Kanren.Core.Micro
      @behaviour Kanren.Core.Mini
    end
  end
end

defmodule Kanren.Core.Nominal do
  # @callback fresh((a -> goal())) :: goal() when a: any()
  # @callback hash()
  # @callback tie()

  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.CLP do
  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.CLP.Tree do
  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.CLP.FD do
  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.Ranked do
  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.Tabled do
  defmacro __using__(_opts) do
    quote do
    end
  end
end

defmodule Kanren.Core.Prob do
  defmacro __using__(_opts) do
    quote do
    end
  end
end



defmodule Kanren.Stream do
  @type t(_a) :: term()

  @callback mzero() :: t(a) when a: any()
  @callback return(a) :: t(a) when a: any()

  @callback mplus(t(a), t(a)) :: t(a) when a: any()
  @callback bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
  @callback msplit(t(a)) :: {a, t(a)} | nil when a: any()
end

defmodule Kanren.Stream.List do
  @behaviour Kanren.Stream

  @type t(a) :: [a]

  @impl Kanren.Stream
  def mzero, do: []

  @impl Kanren.Stream
  def return(x), do: [x]

  @impl Kanren.Stream
  def mplus([], ys), do: ys
  def mplus([x | xs], ys), do: [x | mplus(ys, xs)]
  # def mplus([x | xs], ys), do: [x | mplus(xs, ys)]

  @impl Kanren.Stream
  def bind([], _k), do: mzero()
  def bind([x | xs], k), do: mplus(k.(x), bind(xs, k))

  @impl Kanren.Stream
  def msplit([]), do: nil
  def msplit([x | xs]), do: {x, xs}
end

defmodule Kanren.Stream.Choice do
  @behaviour Kanren.Stream

  @type t(a) :: nil | a | thunk(t(a)) | maybe_improper_list(a, t(a))
  @type thunk(a) :: (-> a)

  @impl Kanren.Stream
  def mzero, do: []

  @impl Kanren.Stream
  def return(a), do: [a]

  @impl Kanren.Stream
  def mplus([], b), do: b
  def mplus(a, b) when is_function(a, 0), do: fn -> mplus(b, a.()) end
  # NOTE: currently implemented alternating/trampoline ^
  #       below is the sequencing implementation...
  # def mplus(a, b) when is_function(a, 0), do: fn -> mplus(a.(), b) end
  def mplus([h | t], b), do: [h | mplus(t, b)]
  def mplus(nil, b), do: mplus([], b)
  def mplus(a, b), do: [a | b]

  @impl Kanren.Stream
  def bind([], _g), do: mzero()
  def bind(a, g) when is_function(a, 0), do: fn -> bind(a.(), g) end
  def bind([h | t], g), do: mplus(g.(h), bind(t, g))
  def bind(nil, g), do: bind([], g)
  def bind(a, g), do: g.(a)

  @impl Kanren.Stream
  def msplit([]), do: nil
  def msplit(a) when is_function(a, 0), do: msplit(a.())
  def msplit([h | t]), do: {h, t}
end



defmodule Kanren.Substitutions do
  @type t() :: term()
  @type lterm() :: term()
  @type lvar(_a) :: term()
  @type lvar() :: lvar(term())

  @callback lvar?(term()) :: boolean()
  @callback occurs?(t(), lterm(), lterm()) :: boolean()
  @callback extend_subs(t(), lterm(), lterm()) :: t()
  @callback lookup_sub(t(), lvar()) :: {:ok, lterm()} | :error
  @callback fresh_lvar(t(), atom()) :: {lvar(), t()}
end

defmodule Kanren.Substitutions.Micro do
  @behaviour Kanren.Substitutions

  @type t() :: {map(), integer()}
  @type lvar(a) :: {a}
  @type lvar() :: lvar(integer())

  @impl Kanren.Substitutions
  def lvar?(x), do: match?({_}, x)

  @impl Kanren.Substitutions
  def occurs?(_s, _u, _v), do: false

  @impl Kanren.Substitutions
  def extend_subs({s, c}, u, v), do: {Map.put(s, u, v), c}

  @impl Kanren.Substitutions
  def lookup_sub({s, _}, u), do: Map.fetch(s, u)

  @impl Kanren.Substitutions
  def fresh_lvar(s, name \\ nil)
  def fresh_lvar({s, c}, nil), do: {lvar(c), {s, c + 1}}
  def fresh_lvar(s, _name), do: fresh_lvar(s, nil)

  @spec lvar(a) :: lvar(a) when a: any()
  defp lvar(a), do: {a}
end



# NOTE: can we do (reify|unify|occur-check)-term like walk-term?
#       (i.e. passing a function to it? does this even make sense?)

# TODO:  walk(logic_term(), State.t()) :: State.t()
# TODO: reify(logic_term(), State.t()) :: State.t()
# TODO: unify(logic_term(), logic_term(), State.t()) :: State.t() | nil

defmodule Kanren.Micro.V2 do
  use Kanren.Core,
    stream: Kanren.Stream.Choice,
    substitutions: Kanren.Substitutions.Micro
  use Kanren.Core.Micro

  @type s() :: @substitutions.t()
  @type goal(a) :: (a -> @stream.t(a))
  @type goal() :: goal(s())
  @type lterm() :: Kanren.Substitutions.lterm()

  # Public API
  ###############

  @impl Kanren.Core.Micro
  def eq(u, v) do
    fn {s, c} ->
      case unify(u, v, s) do
        nil -> @stream.mzero()
        s -> @stream.return({s, c})
      end
    end
  end

  @impl Kanren.Core.Micro
  def exist(gc) do
    fn s ->
      {lvar, s} = @substitutions.fresh_lvar(s)
      gc.(lvar).(s)
    end
  end

  @impl Kanren.Core.Micro
  def conj(g1, g2), do: fn sc -> @stream.bind(g1.(sc), g2) end

  @impl Kanren.Core.Micro
  def disj(g1, g2), do: fn sc -> @stream.mplus(g1.(sc), g2.(sc)) end

  # Interal
  ############

  @spec unify(lterm(), lterm(), s()) :: s() | nil
  defp unify(u, v, s) do
    {walk(u, s), walk(v, s)}
    |> then(fn {u, v} ->
      {u, @substitutions.lvar?(u), v, @substitutions.lvar?(v)}
    end)
    |> case do
      {x, true, x, true} -> s
      {u, true, v, _} -> @substitutions.extend_subs(s, u, v)
      {u, _, v, true} -> @substitutions.extend_subs(s, v, u)
      {[uh | ut], _, [vh | vt], _} ->
        case unify(uh, vh, s) do
          nil -> nil
          s -> unify(ut, vt, s)
        end
      _ -> s
    end
  end

  @spec walk(lterm(), s()) :: lterm()
  defp walk(u, s) do
    with true <- @substitutions.lvar?(u),
         {:ok, val} <- @substitutions.lookup_sub(s, u) do
      walk(val, s)
    else
      _ -> u
    end
  end
end

defmodule Kanren.Micro.V1 do
  @type t(a) :: thunk(t(a)) | maybe_improper_list(a, thunk(t(a)))
  @type t() :: t(state())

  @type goal(a) :: (a -> t(a))
  @type goal() :: goal(state())
  @type state() :: {substitutions :: map(), counter :: integer()}

  @type lvar(a) :: {a}
  @type lvar() :: lvar(integer())
  @type thunk(a) :: (-> a)
  @type walkable(_a) :: term()

  # Public API
  ###############

  @spec eq(walkable(a), walkable(b)) :: goal() when a: any(), b: any()
  def eq(u, v) do
    fn {s, c} ->
      case unify(u, v, s) do
        nil -> mzero()
        s -> unit({s, c})
      end
    end
  end

  @spec exist((a -> goal())) :: goal() when a: any()
  def exist(gc), do: fn {s, c} -> gc.(lvar(c)).({s, c + 1}) end

  @spec conj(goal(), goal()) :: goal()
  def conj(g1, g2), do: fn sc -> bind(g1.(sc), g2) end

  @spec disj(goal(), goal()) :: goal()
  def disj(g1, g2), do: fn sc -> mplus(g1.(sc), g2.(sc)) end

  # Interal
  ############

  @spec lvar(a) :: lvar(a) when a: any()
  def lvar(a), do: {a}

  @spec lvar?(any()) :: boolean()
  def lvar?({_}), do: true
  def lvar?(_), do: false

  @spec unit(a) :: t(a) when a: any()
  def unit(a), do: [a]

  @spec mzero() :: t(a) when a: any()
  def mzero, do: []

  @spec mplus(t(), t()) :: t()
  def mplus([], s), do: s
  def mplus(gs1, gs2) when is_function(gs1, 0), do: fn -> mplus(gs2, gs1.()) end
  def mplus([h | t], gs2), do: [h | mplus(gs2, t)]

  @spec bind(t(), goal()) :: t()
  def bind([], _), do: mzero()
  def bind(gs, g) when is_function(gs, 0), do: fn -> bind(gs.(), g) end
  def bind([h | t], g), do: mplus(g.(h), bind(t, g))

  @spec unify(walkable(a), walkable(b), state()) :: state() | nil when a: any(), b: any()
  def unify(u, v, s) do
    case {walk(u, s), walk(v, s)} do
      {{x}, {x}} -> s
      {{_} = u, v} -> ext_s(u, v, s)
      {u, {_} = v} -> ext_s(v, u, s)
      {[uh | ut], [vh | vt]} ->
        case unify(uh, vh, s) do
          nil -> nil
          s -> unify(ut, vt, s)
        end
      _ -> s
    end
  end

  @spec walk(walkable(a), map()) :: walkable(a) when a: any()
  def walk(u, s) do
    with true <- lvar?(u),
         {:ok, val} <- Map.fetch(s, u) do
      walk(val, s)
    else
      _ -> u
    end
  end

  @spec ext_s(a, b, map()) :: map() when a: any(), b: any()
  def ext_s(u, v, s), do: Map.put(s, u, v)

  defmodule Mini do
    alias Kanren.Micro.V1, as: MK
    alias Kanren.Micro.V1.Mini

    # TODO: ifa + ifu -> conda, condu
    #       (or "if-then" + "or-else"?)

    @spec conj_many([MK.goal()]) :: MK.goal()
    def conj_many([g]), do: g
    def conj_many([g | gs]), do: MK.conj(g , conj_many(gs))

    @spec disj_many([MK.goal()]) :: MK.goal()
    def disj_many([g]), do: g
    def disj_many([g | gs]), do: MK.disj(g , disj_many(gs))

    @spec exist((... -> MK.goal())) :: MK.goal()
    def exist(f) when is_function(f),
      do: do_exist(Function.info(f)[:arity], f, [])

    @spec do_exist(non_neg_integer(), (... -> MK.goal()), [...]) :: MK.goal()
    defp do_exist(0, f, args), do: apply(f, :lists.reverse(args))
    defp do_exist(n, f, args), do: MK.exist(&do_exist(n - 1, f, [&1 | args]))

    @spec conde([[MK.goal()]]) :: MK.goal()
    def conde(clauses) do
      clauses
      |> Enum.map(&conj_many/1)
      |> disj_many()
    end

    @spec run(keyword(), (... -> MK.goal())) :: [term()]
    def run(opts \\ [], f) do
      n = Keyword.get(opts, :n, :infinity)
      exist(f)
      |> call_goal()
      |> take(n)
      |> Enum.map(&reify_1st/1)
    end

    @spec empty_s() :: MK.state()
    def empty_s, do: {%{}, 0}

    @spec call_goal(MK.goal()) :: MK.t()
    def call_goal(g), do: g.(empty_s())

    @spec take(MK.t(), non_neg_integer() | :infinity) :: [MK.state()]
    def take(_, 0), do: MK.mzero()
    def take(x, n) do
      case pull(x) do
        [h | t] -> [h | take(t, dec(n))]
        _ -> []
      end
    end

    defp pull(f) when is_function(f, 0), do: f.()
    defp pull(f), do: f

    @spec dec(integer() | :infinity) :: integer() | :infinity
    defp dec(:infinity), do: :infinity
    defp dec(n), do: n - 1

    @spec walk_all(term(), map()) :: term()
    def walk_all(v, s) do
      v = MK.walk(v, s)
      cond do
        MK.lvar?(v) -> v
        match?([_ | _], v) -> [walk_all(hd(v), s) | walk_all(tl(v), s)]
        true -> v
      end
    end

    def reify_1st({s, _c}) do
      v = walk_all(MK.lvar(0), s)
      walk_all(v, reify_s(v, %{}))
    end

    def reify_s(v, s) do
      v = MK.walk(v, s)
      cond do
        MK.lvar?(v) -> MK.ext_s(v, reify_name(map_size(s)), s)
        match?([_ | _], v) -> reify_s(tl(v), reify_s(hd(v), s))
        true -> s
      end
    end

    defp reify_name(n), do: :"_.#{n}"

    defmodule Macros do
      alias Kanren.Micro.V1.Mini
      alias Kanren.Micro.V1.Mini.Macros, as: M

      defmacro snooze(f) do
        quote do: fn s -> fn -> unquote(f).(s) end end
      end

      defmacro conj_many!(goals) do
        quote do
          unquote(goals) |> Enum.map(&M.snooze/1) |> Mini.conj_many()
        end
      end

      defmacro disj_many!([do: block]) do

      end
      defmacro disj_many!(goals) do
        quote do
          unquote(goals) |> Enum.map(&M.snooze/1) |> Mini.disj_many()
        end
      end

      defmacro exist!(bindings, [do: block]) do
        quote do
          Mini.exist(fn unquote_splicing(bindings) ->

          end)
        end
      end

      defmacro conde!(clauses) do
        quote do
        end
      end

      defmacro run!(bindings, opts \\ [], goals) do
        quote do
          Mini.run()
        end
      end
    end

    defmodule Goals do
    end
  end
end
