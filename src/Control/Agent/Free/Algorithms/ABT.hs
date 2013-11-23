{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Algorithms.ABT
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Family of ABT algorithms.
---------------------------------------------------------------------------
module Control.Agent.Free.Algorithms.ABT where

import Control.Arrow
import Control.Agent.Free
import Control.Lens
import Control.Monad.Trans.Free
import Control.Monad.Free.TH

import qualified Data.Foldable as F

import Data.Map (Map)
import qualified Data.Map as Map

data Message i v
  = MsgOk i v
  | MsgNoGood i (NoGood i v)
  | MsgStop
  deriving (Show)

data NoGood i v = NoGood
  { ngdLHS :: Map i v
  , ngdRHS :: (i, v)
  }
  deriving (Show)

data ABTKernelF i v next
  = SendOk i v next
  | SendBacktrack i (NoGood i v) next
  | SendStop next
  | Recv (Message i v -> next)
  deriving (Functor)
makeFree ''ABTKernelF

type AgentView i v = Map i v

newtype Constraint i v = Constraint
  { constraintCheck  :: AgentView i v -> v -> Maybe (NoGood i v) }

data AgentState i v = AgentState
  { _agStop        :: Bool
  , _agValue       :: Maybe v
  , _agDomain      :: [v]
  , _agId          :: i
  , _agView        :: Map i v
  , _agAbove       :: [i]
  , _agBelow       :: [i]
  , _agConstraints :: [Constraint i v]
  , _agNoGoods     :: [NoGood i v]
  }
makeLenses ''AgentState
