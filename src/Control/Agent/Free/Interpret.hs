{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Interpret
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Rank2Types, MPTCs)
--
-- Interpreting free agent programs.
---------------------------------------------------------------------------
module Control.Agent.Free.Interpret where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Free
import Control.Agent.Free.Internal
import Control.Monad (void)

-- | Interpretation of free monadic values.
class (Functor f, Functor m, Monad m) => MonadInterpret f m where
    -- | Interpret single command.
    interpretF :: f (m a) -> m a
    -- | Interpret complete free monadic value.
    interpretM :: Free f a -> m a
    interpretM = iterM interpretF

-- | Like 'interpretM', but throws away the result.
interpretM_ :: (MonadInterpret f m) => Free f a -> m ()
interpretM_ = void . interpretM

-- | Execute an agent program with particular interpreter.
execAgent :: (Monad (t (Free f)), MonadTrans t, MonadInterpret f m) => (forall n b. Monad n => t n b -> n b) -> Agent t f a -> m a
execAgent run = interpretM . run . runAgent

-- | Execute an agent without exposed structure. See 'execAgent'.
execAgent' :: (MonadInterpret f m) => Agent' f a -> m a
execAgent' = execAgent runIdentityT

-- | Like 'execAgent', but throws away the result.
execAgent_ :: (Monad (t (Free f)), MonadTrans t, MonadInterpret f m) => (forall n b. Monad n => t n b -> n b) -> Agent t f a -> m ()
execAgent_ run = void . execAgent run
