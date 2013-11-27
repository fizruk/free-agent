{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Interfaces.DirectoryService
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- A family of Agent Directory Service (ADS) interfaces.
---------------------------------------------------------------------------
module Control.Agent.Free.Interfaces.DirectoryService (
  -- * Simple ADS
    SimpleADS(..)
  , getId
  , getNeighbors
) where

import Control.Monad.Free.Class

-- | Abstract interface for a simple ADS.
-- @i@ is the type of agent ID.
data SimpleADS i next
  = -- | Ask for an ID.
    GetId (i -> next)
    -- | Ask for IDs of current neighbors.
  | GetNeighbors ([i] -> next)
  deriving (Functor)

-- | Ask for an ID.
getId :: MonadFree (SimpleADS i) m => m i
getId = liftF $ GetId id

-- | Ask for current neighbors' IDs.
getNeighbors :: MonadFree (SimpleADS i) m => m [i]
getNeighbors = liftF $ GetNeighbors id

