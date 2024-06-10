defmodule Logic.T do
  # @type identity(a) :: a

  # @type logic(a) :: logic_t(identity, a)
  # @type logic_t(m, a) :: ((a, m(r) -> m(r)), m(r) -> m(r)) when r: any()
  #   => Functor, Applicative, Alternative, Monad, MonadPlus, MonadLogic, MonadTrans

  # @type state(s, a) :: state_t(s, identity, a)
  # @type state_t(s, m, a) :: (s -> m({a, s}))
  #   => Functor, Applicative, Alternative, Monad, MonadPlus, MonadLogic, MonadTrans

  # @type maybe(a) :: {:just, a} | :nothing

  # @type lvar(a) :: {:lvar, integer(), atom(), maybe(a)}
  # @type store() :: %{optional(integer()) => lvar(a)}
  # @type logic_var_t(m, a) :: state_t(lvar_data, m, a)
  #   => Functor, Applicative, Alternative, Monad, MonadPlus, MonadLogic, MonadTrans

  # @type minikanren(a) :: minikanren_t(identity, a)
  # @type minikanren_t(m, a) :: logic_var_t(logic_t(m), a)
  #   => Functor, Applicative, Alternative, Monad, MonadPlus, MonadLogic, MonadTrans MonadKanren
end

defmodule Logic do
  use Boundary, top_level?: true, deps: [], exports: []

  # @type identity(a) :: a

  # @type logic(a) :: logic_t(identity, a)
  # @type logic_t(m, a) :: ((a, m(r) -> m(r)), m(r) -> m(r)) when r: any()
  #   => Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic, MonadTrans

  # @type state(s, a) :: state_t(s, identity, a)
  # @type state_t(s, m, a) :: (s -> m({a, s}))
  #   => Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic, MonadTrans, MonadState

  # @type maybe(a) :: {:just, a} | :nothing

  # @type lvar(a) :: {:lvar, integer(), atom(), maybe(a)}
  # @type store() :: %{optional(integer()) => lvar(a)}
  # @type logic_var_t(m, a) :: state_t(lvar_data, m, a)
  #   => Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic, MonadTrans

  # @type minikanren(a) :: minikanren_t(identity, a)
  # @type minikanren_t(m, a) :: logic_var_t(logic_t(m), a)
  #   => Functor, Applicative, Monad, Alternative, MonadPlus, MonadLogic, MonadTrans MonadKanren


  # class (Data a) => Unifiable a where
  #   unifyValue    :: (Monad m) => a -> a -> MiniKanrenT m ()
  # class (MonadLogic m) => MonadKanren m where
  #   freshLVar     :: (Data a) => m (LVar a)
  #   conde         :: [m ()] -> m ()
  #   newLVar       :: (Data a) => a -> m (LVar a)
  #   unifyLVar     :: (Unifiable a) => LVar a -> LVar a -> m ()
  #   successful    :: m ()
  #   unsuccessful  :: m ()

  # TODO: MonadLogic + Implementations (i.e. List, StateT, LogicT, ListT, SeqT, LogicStateT, StreamT)
  #   class MonadTrans t where
  #     lift :: Monad m => m a -> t m a
  #   class Monad m => MonadState s m | m -> s where
  #     get     :: m s
  #     put     :: s -> m ()
  #     state   :: (s -> (a, s)) -> m a
  #   class (Monad m, Alternative m) => MonadLogic m where
  #     msplit      :: m a -> m (Maybe (a, m a))
  #     interleave  :: m a -> m a -> m a
  #     (>>-)       :: m a -> (a -> m b) -> m b
  #     once        :: m a -> m a
  #     lnot        :: m a -> m ()
  #     ifte        :: m a -> (a -> m b) -> m b -> m b
  #   class (Alternative m, Monad m) => MonadPlus m where
  #     mzero   :: m a
  #     mplus   :: m a -> m a -> m a
  #   class Applicative f => Alternative f where
  #     empty   :: f a
  #     (<|>)   :: f a -> f a -> f a
  #     some    :: f a -> f [a]
  #     many    :: f a -> f [a]
  #   class Applicative m => Monad m where
  #     return  :: a -> m a
  #     (>>=)   :: forall a b. m a -> (a -> m b) -> m b
  #     (>>)    :: forall a b. m a -> m b -> m b
  #   class Functor f => Applicative f where
  #     pure    :: a -> f a
  #     (<*>)   :: f (a -> b) -> f a -> f b
  #     liftA2  :: (a -> b -> c) -> f a -> f b -> f c
  #     (*>)    :: f a -> f b -> f b
  #     (<*)    :: f a -> f b -> f a
  #   class Functor f where
  #     fmap    :: (a -> b) -> f a -> f b
  #     (<$)    :: a -> f b -> f a

  # TODO: MiniKanren Core
  #   - run/3                             <- LVar, Unification
  #   - Pure
  #     - eq/2                            <- Unification
  #     - conde/1                         <- conde/2-4, all/1-3, other/2-3, bind/2, mplus/2, fail/0
  #     - fresh/2                         <- LVar
  #   - Impure
  #     - conda/1                         <- conda/2-4, all/1-3, other/2-3, bind/2, mplus/2, fail/0
  #     - condu/1                         <- condu/2-4, all/1-3, other/2-3, bind/2, mplus/2, fail/0
  #     - project/2                       <- fresh/2
  #   - Goals
  #     - succeed/1, fail/1               <- N/A
  #     - conso/3, emptyo/1               <- eq/2
  #     - heado/2, tailo/2, appendo/3     <- eq/2, fresh/2
  #     - membero/2                       <- eq/2, fresh/2, conde/1
  #     - onceo/1                         <- condu/1
  #     - copy_term/2, is/3               <- eq/2, project/2
  #     - fresho/1, staleo/1              <- walk/2, lvar?/1
end


# uuid <- specific to each tabled goal                   / set of cached results
# Substitutions.ts = atom(%{uuid => atom(%{[...] => atom(answer_cache)})})
#                                            ^ reified & "cleaned" goal args

# Stream
#   Sequential vs Alternating
#   Logic Behaviour -> MonadLogic, Monad, MonadPlus/Alternative
#                      (List, improper_thunk, LazyTree, LogicT, StateT)
# LVars & State
#   LVar Behaviour
#   Unification Behaviour
#   State Behaviour
# Kanren Behaviour
# Tabling

# uKanren
#   @type lvar(a) :: {a}
#   @type lvar() :: lvar(integer())
#   @type state() :: {substitutions :: map(), counter :: integer()}
#   @type thunk(a) :: (-> a)
#   @type t(a) :: thunk(t(a)) | maybe_improper_list(a, thunk(t(a)))
#   @type goal(a) :: (a -> t(a))
#   @type goal() :: goal(state())
#   @spec eq(walkable(a), walkable(b)) :: goal() when a: any(), b: any()
#   @spec fresh((a -> goal())) :: goal()
#   @spec conj(goal(), goal()) :: goal()
#   @spec disj(goal(), goal()) :: goal()
#   @specp bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
#   @specp mplus(t(a), t(a)) :: t(a) when a: any()
#   @specp unify(walkable(a), walkable(b), state()) :: state() | nil when a: any(), b: any()
#   @specp walk(walkable(a), state()) :: walkable(a) when a: any()
# mKanren
# cKanren
# pKanren
# aKanren


# logic variables
# unification
# pure operators    -> eq, fresh, conde
# impure operators  -> conda, condu, project
# constraints -> CLP(Tree) & CLP(FD) & ...
# nominal
# probabalistic?
# tabling
# "DBs"

# TODO: MicroKanren (uKanren)
#   fresh :: Goal   -- ^ Constructs a new variable binding
#   conj  :: Goal   -- ^ Logical operator - Conjunction
#   disj  :: Goal   -- ^ Logical operator - Disjunction
#   (===) :: Goal   -- ^ Logical operator - Equality

# -- | (X = "1" OR X = "2") AND (Y = "a" OR Y = "b")
# goal :: Goal
# goal = fresh $
#     \x -> fresh $
#         \y -> ((x === Atom "1") `disj` (x === Atom "2"))
#             `conj` ((y === Atom "a") `disj` (y === Atom "b"))

# initialState :: State
# initialState = ([], 0)

# results :: Stream State
# results = goal initialState

# displayResults :: IO ()
# displayResults = putStrLn $ prettyPrintResults results

# -- | (X = "1" OR X = "2") AND (Y = "a" OR Y = "b")
# -- Var 0 := X
# -- Var 1 := Y

# Var 1 := Atom "a"
# Var 0 := Atom "1"

# Var 1 := Atom "a"
# Var 0 := Atom "2"

# Var 1 := Atom "b"
# Var 0 := Atom "1"

# Var 1 := Atom "b"
# Var 0 := Atom "2"

# type State = (Subst, VariableCounter)

# initialState = ([], 0)
# Gets updated to…
# updatedState = ([(Var 0, Atom "1")], 1)

# data Stream a = Nil
#               | Cons a (Stream a)
#               | Delayed (Stream a) deriving (Eq, Show)


# TODO: MiniKanren  (mKanren)
# TODO: AlphaKanren (aKanren)
