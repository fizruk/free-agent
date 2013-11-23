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

type A i v a = forall m. Monad m => StateT (AgentState i v) (Agent' (ABTKernelF i v) m) a

prg :: (Ord i, Eq v) => A i v (Maybe v)
prg = do
  checkAgentView
  msgLoop
  use agValue

msgLoop :: (Ord i, Eq v) => A i v ()
msgLoop = do
  stop <- use agStop
  unless (stop <= 0) $ do
    msg <- recv
    case msg of
      MsgOk src val -> do
        agentUpdate src (Just val)
        checkAgentView
      MsgNoGood src ngd -> do
        resolveConflict src ngd
      MsgStop -> do
        agStop .= False
    msgLoop

-- | Resolve conflict by
resolveConflict :: (Ord i, Eq v) => i -> NoGood i v -> A i v ()
resolveConflict sender ngd = do
  view <- use agView
  if coherent (ngdLHS ngd) view then do
    agNoGoods %= (ngd:)
    agValue .= Nothing
    checkAgentView
  else do
    val <- use agValue
    let val' = Just . snd . ngdRHS $ ngd
    when (val == val') $ do
      sendOk sender val

-- | Stop and send STOP message.
-- The solution does not exist.
stopAgent :: A i v ()
stopAgent = do
  agStop .= False
  sendStop

-- | Update agent's view.
agentUpdate :: i -> Maybe v -> A i v ()
agentUpdate src val = do
  agView.at src .= val
  view <- use agView
  agNoGoods %= filter (coherent view . ngdLHS)

-- | Check whether NoGood is coherent with agent's view (i.e. whether
-- nogood's LHS does not contain old information about values).
coherent :: (Ord a, Eq b) => Map a b -> Map a b -> Bool
coherent ma mb = F.and $ Map.intersectionWith (==) ma mb

-- | Recheck whether current view is consistent with agent's value.
-- If not - try rechoose value.
checkAgentView :: A i v ()
checkAgentView = do
  val  <- gets agentValue
  view <- gets agentView
  c    <- consistent val view
  unless c $ do
    val <- chooseValue
    agValue .= val
    case val of
      -- unable to choose a value
      Nothing -> do
        backtrack
      -- value chosen
      Just x  -> do
        ids <- use agAbove
        mapM_ (flip sendOk x) ids

-- | Try to choose a value w.r.t. nogood store, constraints
-- and current view.
chooseValue :: A i v (Maybe v)
chooseValue = do
  -- eliminate values by nogood store
  xs   <- use agDomain >>= eliminateNoGoods -- FIXME: too strict
  -- check the rest for consistency with constraints
  view <- use agView
  cs   <- uses agConstraints $ map (flip constraintCheck view)
  let (ngds, v) = choose (msum . zipWith ($) cs . repeat) xs
  agNoGoods %= (ngds ++)
  return v
  where
    choose _ [] = ([], Nothing)
    choose f (x:xs) =
      case f x of
        Just y  -> first (y:) (choose xs)
        Nothing -> ([], Just x)

-- | Eliminate values taken by nogoods.
eliminateNoGoods :: Eq v => [v] -> A i v [v]
eliminateNoGoods xs = do
  ys <- uses agNoGoods $ map (snd . ngdRHS)
  return $ xs \\ nub ys

-- | Check value for consistency with current view and constraints.
-- Returns the list of constraints that would be broken by choosing the value.
consistent :: Maybe v -> A i v Bool
consistent Nothing  = return (False, [])
consistent (Just x) = do
  view <- use agView
  uses agConstraints $ all . isNothing . (\c -> checkConstraint c view x)
