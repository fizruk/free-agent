{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Internal
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Internals of the free agent.
---------------------------------------------------------------------------
module Control.Agent.Free where

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity

-- | An @Agent f t a@ is a program which uses functor @f@ as a low-level
-- API, monad transformer @t@ — for high-level features and which returns
-- a value of type @a@.
--
-- The important thing is that the environment may mess with @t@ on
-- initialization of the agent or on low-level API calls. This was designed
-- for the possibility of `behaviosites' (see
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.114.4071> for
-- more details).
type Agent f t (m :: * -> *) = FreeT f (t m)

-- | An 'Agent' without any exposed structure.
type Agent' f m = Agent f IdentityT m

-- | A constraint synonym to make type signatures less scary.
type FMT f m t = (Functor f, Monad m, MonadTrans t, Monad (t m))

-- | @'transform' phi agent@ applies @phi@ to each low-level API command
-- in @agent@ program. This is the basis for `behaviosites'.
transform :: (FMT f m t) => (f (t m a) -> t m a) -> FreeT f m a -> t m a
transform f = iterT f . hoistFreeT lift

-- | Execute an agent program with particular interpreter.
-- @'execAgent' int agent@ give an interpretation to the low-level API.
execAgent :: (FMT f m t) => (forall b. f (m b) -> m b) -> Agent f t m a -> t m a
execAgent f = iterT (join . lift . f . fmap return)

-- | Like 'execAgent' for agents with no exposed structure.
execAgent' :: (Functor f, Monad m) => (forall b. f (m b) -> m b) -> Agent' f m a -> m a
execAgent' f = runIdentityT . execAgent f

-- | Like 'execAgent' but discards the result.
execAgent_ :: (FMT f m t) => (forall b. f (m b) -> m b) -> Agent f t m a -> t m ()
execAgent_ f = liftM (const ()) . execAgent f

-- | Like 'execAgent'' but discards the result.
execAgent'_ :: (Functor f, Monad m) => (forall b. f (m b) -> m b) -> Agent' f m a -> m ()
execAgent'_ f = runIdentityT . execAgent_ f

