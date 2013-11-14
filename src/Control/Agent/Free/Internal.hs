{-# LANGUAGE RankNTypes #-}
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
module Control.Agent.Free.Internal where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Data.Functor.Identity

-- | An @Agent f t a@ is a program which uses functor @f@ as a low-level
-- API, monad transformer @t@ — for high-level features and which returns
-- a value of type @a@.
--
-- The important thing is that the environment may mess with @t@ on
-- initialization of the agent or on low-level API calls. This was designed
-- for the possibility of `behaviosites' (see
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.114.4071> for
-- more details).
type Agent f t = FreeT f (t Identity)

-- | An 'Agent' without any exposed structure.
type Agent' f = Agent f IdentityT

-- | @'transform' phi agent@ applies @phi@ to each low-level API command
-- in @agent@ program. This is the basis for `behaviosites'.
transform :: (MonadTrans t, Monad m, Functor f, Monad (t m), Monad (t Identity)) => (f (Agent f t a) -> Agent f t a) -> Agent f t a -> Agent f t a
transform = transformFreeT

-- | Execute an agent program with particular interpreter.
-- @'execAgent' int agent@ give an interpretation to the low-level API.
execAgent :: (MFunctor t, MonadTrans t, Monad m, Functor f, Monad (t m), Monad (t Identity)) => (f (t m a) -> t m a) -> Agent f t a -> t m a
execAgent f = iterT f . hoistFreeT (hoist $ return . runIdentity)

-- | Like 'execAgent' for agents without exposed structure.
execAgent' :: (Monad m, Functor f) => (f (m a) -> m a) -> Agent' f a -> m a
execAgent' f = runIdentityT . execAgent (lift . f . fmap runIdentityT)

-- | Like 'execAgent' but discards the result.
execAgent_ :: (MFunctor t, MonadTrans t, Monad m, Functor f, Monad (t m), Monad (t Identity)) => (f (t m a) -> t m a) -> Agent f t a -> t m ()
execAgent_ f = liftM (const ()) . execAgent f

-- | Like 'execAgent'' but discards the result.
execAgent'_ :: (Monad m, Functor f) => (f (m a) -> m a) -> Agent' f a -> m ()
execAgent'_ f = liftM (const ()) . execAgent' f

-- | This should be in @Control.Monad.Trans.Free@.
transformFreeT :: (MonadTrans t, Monad m, Functor f, Monad (t m)) => (f (t m a) -> t m a) -> FreeT f m a -> t m a
transformFreeT f = iterT f . hoistFreeT lift

