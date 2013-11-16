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
type Agent f t a = forall m. Monad m => FreeT f (t m) a

-- | An 'Agent' without any exposed structure.
type Agent' f a = Agent f IdentityT a

-- | @'transform' phi agent@ applies @phi@ to each low-level API command
-- in @agent@ program. This is the basis for `behaviosites'.
transform :: (Functor f, Monad m, MonadTrans t, Monad (t m)) => (f (t m a) -> t m a) -> FreeT f m a -> t m a
transform f = iterT f . hoistFreeT lift

-- | Execute an agent program with particular interpreter.
-- @'execAgent' int agent@ give an interpretation to the low-level API.
execAgent :: (Functor f, Monad m, MonadTrans t, Monad (t m)) => (forall b. f (m b) -> m b) -> FreeT f (t m) a -> t m a
execAgent f = iterT (join . lift . f . fmap return)

-- | Like 'execAgent' but discards the result.
execAgent_ :: (Functor f, Monad m, MonadTrans t, Monad (t m)) => (forall b. f (m b) -> m b) -> FreeT f (t m) a -> t m ()
execAgent_ f = liftM (const ()) . execAgent f

