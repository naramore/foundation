defmodule Class do
  # require Logger

  @spec id(x) :: x when x: any()
  def id(x), do: x

  @spec const(a, b) :: a when a: any(), b: any()
  def const(a, _b), do: a

  @spec comp((b -> c), (a -> b)) :: (a -> c) when a: any(), b: any(), c: any()
  def comp(f, g), do: fn x -> x |> g.() |> f.() end

  @callback dispatch(list()) :: boolean()

  @spec dispatch!(module(), atom(), list()) :: term() | no_return()
  def dispatch!(mod, fun, args) do
    error = {:error, dispatch_error(mod, fun, args)}

    mod.instances()
    # |> tap(fn ms -> Logger.debug(fn -> %{checking_modules: ms, mfa: {__MODULE__, :dispatch!, 3}, args: [mod: mod, fun: fun, args: args]} end) end)
    |> Enum.reduce_while(error, fn m, acc ->
      {d, m} = get_dispatcher(mod, m)
      if d.(args) do
        # _ = Logger.debug(fn -> %{found_it!: m, mfa: {__MODULE__, :dispatch!, 3}, args: [mod: mod, fun: fun, args: args]} end)
        {:halt, {:ok, apply(m, fun, args)}}
      else
        # _ = Logger.debug(fn -> %{not_it: m, mfa: {__MODULE__, :dispatch!, 3}, args: [mod: mod, fun: fun, args: args]} end)
        {:cont, acc}
      end
    end)
    |> case do
      {:ok, x} -> x
      {:error, error} -> raise error
    end
  end

  @spec default_dispatcher?(module()) :: boolean()
  def default_dispatcher?(mod), do: mod in Map.keys(default_dispatches())

  @spec get_dispatcher(module(), module(), %{required(module()) => (list() -> boolean())}) :: {(list() -> boolean()), module()}
  defp get_dispatcher(mod, m, defaults \\ default_dispatches()) do
    case Map.get(defaults, m, {&m.dispatch/1, false}) do
      {d, false} -> {d, m}
      d -> {d, Module.concat(mod, m)}
    end
  end

  @spec default_dispatches() :: %{required(module()) => (list() -> boolean())}
  defp default_dispatches do
    %{
      List => &is_list(hd(&1)),
      Tuple => &is_tuple(hd(&1)),
      Map => &is_map(hd(&1)),
      Function => &is_function(hd(&1)),
      MapSet => &match?([%MapSet{} | _], &1),
      BitString => &is_bitstring(hd(&1)),
      Float => &is_float(hd(&1)),
      Integer => &is_integer(hd(&1)),
    }
  end

  @spec dispatch_error(module(), atom(), list()) :: ArgumentError.t()
  defp dispatch_error(m, f, a) do
    %ArgumentError{message: "invalid args for #{m}.#{f}/#{length(a)}: #{inspect(a)}"}
  end

  defmacro __using__(_opts) do
    quote do
      alias Class.{Semigroup, Monoid}
      alias Class.{Functor, Applicative, Monad}
    end
  end
end

defmodule Maybe do
  @moduledoc """
    ```haskell
    data  Maybe a  =  Nothing | Just a

    instance  Functor Maybe  where
      fmap _ Nothing       = Nothing
      fmap f (Just a)      = Just (f a)

    instance Applicative Maybe where
      pure = Just

      Just f  <*> m       = fmap f m
      Nothing <*> _m      = Nothing

      liftA2 f (Just x) (Just y) = Just (f x y)
      liftA2 _ _ _ = Nothing

      Just _m1 *> m2      = m2
      Nothing  *> _m2     = Nothing

    instance  Monad Maybe  where
      (Just x) >>= k      = k x
      Nothing  >>= _      = Nothing

      (>>) = (*>)

    instance Alternative Maybe where
      empty = Nothing
      Nothing <|> r = r
      l       <|> _ = l

    instance MonadPlus Maybe
    ```
  """

  @behaviour Class
  use Class.{Semigroup, Monoid}
  use Class.{Functor, Applicative, Monad}
  use Class.{Alternative, Monad.Fail, Monad.Plus}

  @type t(a) :: :nothing | {:just, a}

  @impl Class
  def dispatch([:nothing | _]), do: true
  def dispatch([{:just, _} | _]), do: true
  def dispatch(_), do: false

  #############################################################################

  @impl Semigroup
  def append(:nothing, b), do: b
  def append({:just, a}, {:just, b}),
    do: a |> Semigroup.append(b) |> just()
  def append({:just, _} = a, :nothing), do: a

  @impl Monoid
  def mempty(_), do: nothing()

  @impl Functor
  def fmap(:nothing, _f), do: nothing()
  def fmap({:just, a}, f), do: just(f.(a))

  @impl Applicative
  def pure(_ta, a), do: just(a)

  @impl Applicative
  def convey(ta, {:just, f}), do: Functor.fmap(ta, f)
  def convey(_ta, :nothing), do: nothing()

  @impl Monad
  def bind(:nothing, _k), do: nothing()
  def bind({:just, m}, k), do: k.(m)

  @impl Alternative
  def empty(a), do: mempty(a)

  @impl Alternative
  def assoc(:nothing, b), do: b
  def assoc(a, _), do: a

  @impl Monad.Fail
  def fail(_, _), do: nothing()

  #############################################################################

  @spec just(a) :: {:just, a} when a: any()
  def just(a), do: {:just, a}

  @spec nothing() :: :nothing
  def nothing, do: :nothing

  @spec maybe(t(a), (a -> b), b) :: b when a: any(), b: any()
  def maybe(m, f, default \\ nil)
  def maybe(:nothing, _f, default), do: default
  def maybe({:just, a}, f, _default), do: f.(a)
end

defmodule Free do
  @moduledoc ~S"""
  FreeF
    ```haskell
    data FreeF f a b = Pure a | Free (f b)
      deriving (Eq,Ord,Show,Read,Data,Generic,Generic1)

    instance Functor f => Functor (FreeF f a) where
      fmap _ (Pure a)  = Pure a
      fmap f (Free as) = Free (fmap f as)

    instance Foldable f => Foldable (FreeF f a) where
      foldMap f (Free as) = foldMap f as
      foldMap _ _         = mempty

    instance Traversable f => Traversable (FreeF f a) where
      traverse _ (Pure a)  = pure (Pure a)
      traverse f (Free as) = Free <$> traverse f as
    ```

  FreeT
    ```haskell
    newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }
    type Free f = FreeT f Identity

    instance (Functor f, Functor m) => Functor (FreeT f m) where
      fmap f (FreeT m) = FreeT (fmap f' m) where
        f' (Pure a)  = Pure (f a)
        f' (Free as) = Free (fmap (fmap f) as)

    instance (Applicative f, Applicative m) => Applicative (FreeT f m) where
      pure a = FreeT (pure (Pure a))
      FreeT f <*> FreeT a = FreeT $ g <$> f <*> a where
        g (Pure f') (Pure a') = Pure (f' a')
        g (Pure f') (Free as) = Free $ fmap f' <$> as
        g (Free fs) (Pure a') = Free $ fmap ($ a') <$> fs
        g (Free fs) (Free as) = Free $ (<*>) <$> fs <*> as

    instance (Applicative f, Monad m) => Monad (FreeT f m) where
      return = pure
      FreeT m >>= f = FreeT $ m >>= \v -> case v of
        Pure a -> runFreeT (f a)
        Free w -> return (Free (fmap (>>= f) w))
      fail e = FreeT (fail e)

    instance (Applicative f, Fail.MonadFail m) => Fail.MonadFail (FreeT f m) where
      fail e = FreeT (Fail.fail e)

    instance Applicative f => MonadTrans (FreeT f) where
      lift = FreeT . liftM Pure

    instance (Applicative f, MonadPlus m) => Alternative (FreeT f m) where
      empty = FreeT mzero
      FreeT ma <|> FreeT mb = FreeT (mplus ma mb)

    instance (Applicative f, MonadPlus m) => MonadPlus (FreeT f m) where
      mzero = FreeT mzero
      mplus (FreeT ma) (FreeT mb) = FreeT (mplus ma mb)

    instance (Applicative f, Monad m) => MonadFree f (FreeT f m) where
      wrap = FreeT . return . Free

    instance (Foldable m, Foldable f) => Foldable (FreeT f m) where
      foldMap f (FreeT m) = foldMap (bifoldMap f (foldMap f)) m

    instance (Monad m, Traversable m, Traversable f) => Traversable (FreeT f m) where
      traverse f (FreeT m) = FreeT <$> traverse (bitraverse f (traverse f)) m
    ```
  """

  # @type freef(f, a, b) :: {:pure, a} | {:free, f(b)}
  # @type freet(f, m, a) :: m(freef(f, a, freet(f, m, a)))
  # @type identity(a) :: a
  # @type free(f, a) :: freet(f, identity, a)

  # @type free(f, a) :: freef(f, a, free(f, a))
  # @type free(f, a) :: {:pure, a} | {:free, f(free(f, a))}
  # @type t(f, a) :: {:pure, a} | {:free, f}
end

defmodule Logic do
  @moduledoc ~S"""
  LogicT & Logic
    ```haskell
    newtype LogicT m a =
      LogicT { unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }

    type Logic = LogicT Identity

    instance Functor (LogicT f) where
      fmap f lt = LogicT $ \sk fk -> unLogicT lt (sk . f) fk

    instance Applicative (LogicT f) where
      pure a = LogicT $ \sk fk -> sk a fk
      f <*> a = LogicT $ \sk fk -> unLogicT f (\g fk' -> unLogicT a (sk . g) fk') fk

    instance Alternative (LogicT f) where
      empty = LogicT $ \_ fk -> fk
      f1 <|> f2 = LogicT $ \sk fk -> unLogicT f1 sk (unLogicT f2 sk fk)

    instance Monad (LogicT m) where
      return = pure
      m >>= f = LogicT $ \sk fk -> unLogicT m (\a fk' -> unLogicT (f a) sk fk') fk
      fail = Fail.fail

    instance Fail.MonadFail (LogicT m) where
      fail _ = LogicT $ \_ fk -> fk

    instance MonadPlus (LogicT m) where
      mzero = empty
      mplus = (<|>)

    instance Semigroup (LogicT m a) where
      (<>) = mplus
      sconcat = F.foldr1 mplus

    instance Monoid (LogicT m a) where
      mempty = empty
      mappend = (<>)
      mconcat = F.asum

    instance MonadTrans LogicT where
      lift m = LogicT $ \sk fk -> m >>= \a -> sk a fk

    instance (Monad m) => MonadLogic (LogicT m) where
        -- 'msplit' is quite costly even if the base 'Monad' is 'Identity'.
        -- Try to avoid it.
        msplit m = lift $ unLogicT m ssk (return Nothing)
          where
            ssk a fk = return $ Just (a, lift fk >>= reflect)
        once m = LogicT $ \sk fk -> unLogicT m (\a _ -> sk a fk) fk
        lnot m = LogicT $ \sk fk -> unLogicT m (\_ _ -> fk) (sk () fk)

    instance {-# OVERLAPPABLE #-} (Applicative m, F.Foldable m) => F.Foldable (LogicT m) where
        foldMap f m = F.fold $ unLogicT m (fmap . mappend . f) (pure mempty)

    instance {-# OVERLAPPING #-} F.Foldable (LogicT Identity) where
        foldr f z m = runLogic m f z

    instance {-# OVERLAPPING #-} T.Traversable (LogicT Identity) where
      traverse g l = runLogic l (\a ft -> cons <$> g a <*> ft) (pure empty)
        where
          cons a l' = pure a <|> l'

    instance {-# OVERLAPPABLE #-} (Monad m, T.Traversable m) => T.Traversable (LogicT m) where
      traverse f = fmap fromML . T.traverse f . toML
    ```

  ML
    ```haskell
    newtype ML m a = ML (m (MLView m a))
      deriving (Functor, F.Foldable, T.Traversable)

    data MLView m a = EmptyML | ConsML a (ML m a)
      deriving (Functor, F.Foldable)

    instance T.Traversable m => T.Traversable (MLView m) where
      traverse _ EmptyML = pure EmptyML
      traverse f (ConsML x (ML m))
        = liftA2 (\y ym -> ConsML y (ML ym)) (f x) (T.traverse (T.traverse f) m)
      {- The derived instance would write the second case as
      -
      -   traverse f (ConsML x xs) = liftA2 ConsML (f x) (traverse @(ML m) f xs)
      -
      - Inlining the inner traverse gives
      -
      -   traverse f (ConsML x (ML m)) = liftA2 ConsML (f x) (ML <$> traverse (traverse f) m)
      -
      - revealing fmap under liftA2. We fuse those into a single application of liftA2,
      - in case fmap isn't free.
      -}

    toML :: Applicative m => LogicT m a -> ML m a
    toML (LogicT q) = ML $ q (\a m -> pure $ ConsML a (ML m)) (pure EmptyML)

    fromML :: Monad m => ML m a -> LogicT m a
    fromML (ML m) = lift m >>= \r -> case r of
      EmptyML -> empty
      ConsML a xs -> pure a <|> fromML xs
    ```
  """

  # @type logict(m, a, r) :: ((a, m(r) -> m(r)), m(r) -> m(r))
  # @type logict(m, a) :: logict(m, a, any())
  # @type identity(a) :: a
  # @type logic(a) :: logict(identity, a)

  # newtype LogicT m a =
  #   LogicT { unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }
  # type Logic = LogicT Identity
  @type logict(m, a, _r) :: ((a, m -> m), m -> m)
  @type logict(m, a) :: logict(m, a, any())

  @type logic(a, r) :: ((a, r -> r), r -> r)
  @type logic(a) :: logic(a, any())

  #############################################################################



  @spec id(x) :: x when x: any()
  def id(x), do: x

  @spec const(a, b) :: a when a: any(), b: any()
  def const(a, _b), do: a

  @spec comp((b -> c), (a -> b)) :: (a -> c) when a: any(), b: any(), c: any()
  def comp(f, g), do: fn x -> x |> g.() |> f.() end

  @spec ap((a -> b), a) :: b when a: any(), b: any()
  def ap(f, a), do: f.(a)

  @spec head(a, [a]) :: [a] when a: any()
  def head(h, t), do: [h | t]

  defdelegate take(xs, n), to: Enum

  #############################################################################

  # runLogicT :: LogicT m a -> (a -> m r -> m r) -> m r -> m r
  # runLogicT (LogicT r) = r

  # observeT :: MonadFail m => LogicT m a -> m a
  # observeT lt = unLogicT lt (const . return) (fail "No answer.")

  # observeManyT :: Monad m => Int -> LogicT m a -> m [a]
  # observeManyT n m
  #     | n <= 0 = return []
  #     | n == 1 = unLogicT m (\a _ -> return [a]) (return [])
  #     | otherwise = unLogicT (msplit m) sk (return [])
  #  where
  #  sk Nothing _ = return []
  #  sk (Just (a, m')) _ = (a:) `liftM` observeManyT (n-1) m'

  # observeAllT :: Applicative m => LogicT m a -> m [a]
  # observeAllT m = unLogicT m (fmap . (:)) (pure [])

  # fromLogicT :: (Alternative (t m), MonadTrans t, Monad m, Monad (t m)) => LogicT m a -> t m a
  # fromLogicT = fromLogicTWith lift

  # fromLogicTWith :: (Applicative m, Monad n, Alternative n) => (forall x. m x -> n x) -> LogicT m a -> n a
  # fromLogicTWith p (LogicT f) = join . p $
  #   f (\a v -> pure (pure a <|> join (p v))) (pure empty)

  # hoistLogicT :: (Applicative m, Monad n) => (forall x. m x -> n x) -> LogicT m a -> LogicT n a
  # hoistLogicT f = fromLogicTWith (lift . f)

  # embedLogicT :: Applicative m => (forall a. m a -> LogicT n a) -> LogicT m b -> LogicT n b
  # embedLogicT f = fromLogicTWith f

  #############################################################################

  # logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a
  # logic f = LogicT $ \k -> Identity .
  #                          f (\a -> runIdentity . k a . Identity) .
  #                          runIdentity

  # runLogic :: Logic a -> (a -> r -> r) -> r -> r
  # runLogic l s f = runIdentity $ unLogicT l si fi
  #   where
  #   si = fmap . s
  #   fi = Identity f

  # observe :: Logic a -> a
  # observe lt = runIdentity $ unLogicT lt (const . pure) (error "No answer.")

  # observeMany :: Int -> Logic a -> [a]
  # observeMany i = L.take i . observeAll

  # observeAll :: Logic a -> [a]
  # observeAll = runIdentity . observeAllT

  #############################################################################

  # reflect :: Alternative m => Maybe (a, m a) -> m a
  # reflect Nothing = empty
  # reflect (Just (a, m)) = pure a <|> m
end
