{-# LANGUAGE Rank2Types #-}

module Control.Agent.Free.Internal (
    Agent(..),
) where

import Control.Monad.Free
import Control.Monad.Trans.Demarcate

-- | An @Agent t f a@ is a program which uses functor @f@ as a low-level
-- API, monad transformer @t@ — for high-level features and which returns
-- a value of type @a@.
--
-- The important thing is that the environment may mess with @t@ on
-- initialization of the agent or on low-level API calls. This was designed
-- for the possibility of `behaviosites' (see
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.114.4071 for
-- more details).
data Agent t f a = Agent
  { agentRun  :: forall m b. (Monad m) => t m b -> m b
  , agentPrg  :: forall m. (MonadFree f m) => Demarcate t m a
  }

