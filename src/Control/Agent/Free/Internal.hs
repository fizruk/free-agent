{-# LANGUAGE Rank2Types #-}
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
  { -- | Default evaluation for @t@ monad transformer.
    agentRun  :: forall m b. (Monad m) => t m b -> m b
  , -- | Program of an agent.
    agentPrg  :: forall m. (MonadFree f m) => Demarcate t m a
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

