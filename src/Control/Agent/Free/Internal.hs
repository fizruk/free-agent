{-# LANGUAGE Rank2Types #-}

module Control.Agent.Free.Internal (
    Agent(..),
    liftCmd,
) where

import Control.Monad.Free.Class
import Control.Monad.Trans.Class

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

-- | Turn an API functor into a polymorphic DSL command for agent
-- programming. An example of typical use:
--
-- @
-- data API next
--    = Output String next
--    | Input (String -> next)
--    deriving (Functor)
--
-- output s = liftCmd $ Output s ()
-- input    = liftCmd $ Input id
-- @
liftCmd :: (Functor f, MonadFree f m, MonadTrans t) => f a -> t m a
liftCmd = lift . liftF

