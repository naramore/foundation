{-# LANGUAGE CPP
           , Rank2Types
           , MultiParamTypeClasses
           , UndecidableInstances
           , FlexibleInstances
           #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                  ~ 2021.11.07
-- |
-- Module      :  Control.Unification.Ranked.STVar
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  highly experimental
-- Portability :  semi-portable (Rank2Types, MPTCs,...)
--
-- A ranked variant of "Control.Unification.STVar".
----------------------------------------------------------------
module Control.Unification.Ranked.STVar
    ( STRVar()
    , STRBinding()
    , runSTRBinding
    ) where

import Prelude hiding (mapM, sequence, foldr, foldr1, foldl, foldl1)

import Data.STRef
import Data.Word            (Word8)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative  (Applicative(..))
#endif
import Control.Monad.Trans  (lift)
import Control.Monad.ST
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Unification.Types
----------------------------------------------------------------
----------------------------------------------------------------

-- | A ranked unification variable implemented by 'STRef's. In
-- addition to the @STRef@ for the term itself, we also track the
-- variable's ID (to support visited-sets) and rank (to support
-- weighted path compression).
data STRVar s t =
    STRVar
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !(STRef s Word8)
        {-# UNPACK #-} !(STRef s (Maybe (UTerm t (STRVar s t))))

instance Show (STRVar s t) where
    show (STRVar i _ _) = "STRVar " ++ show i

instance Eq (STRVar s t) where
    (STRVar i _ _) == (STRVar j _ _) = (i == j)

instance Variable (STRVar s t) where
    getVarID (STRVar i _ _) = i


----------------------------------------------------------------
-- TODO: parameterize this so we can use BacktrackST too. Of course,
-- that means defining another class for STRef-like variables
--
-- TODO: parameterize this so we can share the implementation for STVar and STRVar
--
-- TODO: does MTL still have the overhead that'd make it worthwhile
-- to do this manually instead of using ReaderT?
--
-- | A monad for handling 'STRVar' bindings.
newtype STRBinding s a = STRB { unSTRB :: ReaderT (STRef s Int) (ST s) a }


-- | Run the 'ST' ranked binding monad. N.B., because 'STRVar' are
-- rank-2 quantified, this guarantees that the return value has no
-- such references. However, in order to remove the references from
-- terms, you'll need to explicitly apply the bindings.
runSTRBinding :: (forall s. STRBinding s a) -> a
runSTRBinding m =
    runST (newSTRef minBound >>= runReaderT (unSTRB m))
    -- N.B., because of the rank-2 quantification, cannot use the
    -- 'STRB' pattern in lieu of 'unSTRB' here.


-- For portability reasons, we're intentionally avoiding
-- -XDeriveFunctor, -XGeneralizedNewtypeDeriving, and the like.

instance Functor (STRBinding s) where
    fmap f = STRB . fmap f . unSTRB

instance Applicative (STRBinding s) where
    pure              = STRB . pure
    STRB m <*> STRB n = STRB (m <*> n)
    STRB m  *> STRB n = STRB (m  *> n)
    STRB m <*  STRB n = STRB (m <*  n)

-- Since base-4.8 (ghc-7.10.1) we have the default @return = pure@.
-- Since ghc-9.2.1 we get a warning about providing any other
-- definition, and should instead define both 'pure' and @(*>)@
-- directly, leaving 'return' and @(>>)@ as their defaults so they
-- can eventually be removed from the class.
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return>
instance Monad (STRBinding s) where
#if (!(MIN_VERSION_base(4,8,0)))
    return       = pure
#endif
    STRB m >>= f = STRB (m >>= unSTRB . f)


----------------------------------------------------------------

_newSTRVar
    :: String
    -> Maybe (UTerm t (STRVar s t))
    -> STRBinding s (STRVar s t)
_newSTRVar fun mb = STRB $ do
    nr <- ask
    lift $ do
        n <- readSTRef nr
        if n == maxBound
            then error $ fun ++ ": no more variables!"
            else do
                writeSTRef nr $! n+1
                -- BUG: no applicative ST
                rk  <- newSTRef 0
                ptr <- newSTRef mb
                return (STRVar n rk ptr)


instance (Unifiable t) => BindingMonad t (STRVar s t) (STRBinding s) where
    lookupVar (STRVar _ _ p) = STRB . lift $ readSTRef p

    freeVar  = _newSTRVar "freeVar" Nothing

    newVar t = _newSTRVar "newVar" (Just t)

    bindVar (STRVar _ _ p) t = STRB . lift $ writeSTRef p (Just t)


instance (Unifiable t) =>
    RankedBindingMonad t (STRVar s t) (STRBinding s)
    where

    lookupRankVar (STRVar _ r p) = STRB . lift $ do
        n  <- readSTRef r
        mb <- readSTRef p
        return (Rank n mb)

    incrementRank (STRVar _ r _) = STRB . lift $ do
        n <- readSTRef r
        writeSTRef r $! n+1

    -- incrementBindVar = default

----------------------------------------------------------------
----------------------------------------------------------- fin.
