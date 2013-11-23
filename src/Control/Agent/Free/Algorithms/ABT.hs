{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Applicative
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Free.TH
import Control.Monad.State

import qualified Data.Foldable as F

import Data.List
import Data.Maybe
import qualified Data.Set as Set

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

initialAgentState :: AgentState i v
initialAgentState = AgentState
  { _agStop        = False
  , _agValue       = Nothing
  , _agDomain      = []
  , _agId          = undefined
  , _agView        = Map.empty
  , _agAbove       = []
  , _agBelow       = []
  , _agConstraints = []
  , _agNoGoods     = []
  }

type A i v a = forall m. Monad m => StateT (AgentState i v) (Agent' (ABTKernelF i v) m) a

prg :: (Ord i, Eq v) => A i v (Maybe v)
prg = do
  checkAgentView
  msgLoop
  use agValue

msgLoop :: (Ord i, Eq v) => A i v ()
msgLoop = do
  stop <- use agStop
  unless stop $ do
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
    let val' = snd . ngdRHS $ ngd
    when (val == Just val') $ do
      sendOk sender val'

-- | Stop and send STOP message.
-- The solution does not exist.
stopAgent :: A i v ()
stopAgent = do
  agStop .= False
  sendStop

-- | Update agent's view.
agentUpdate :: (Ord i, Eq v) => i -> Maybe v -> A i v ()
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
checkAgentView :: (Ord i, Eq v) => A i v ()
checkAgentView = do
  c <- use agValue >>= consistent
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
chooseValue :: Eq v => A i v (Maybe v)
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
        Just y  -> first (y:) (choose f xs)
        Nothing -> ([], Just x)

-- | Eliminate values taken by nogoods.
eliminateNoGoods :: Eq v => [v] -> A i v [v]
eliminateNoGoods xs = do
  ys <- uses agNoGoods $ map (snd . ngdRHS)
  return $ xs \\ nub ys

-- | Check value for consistency with current view and constraints.
-- Returns the list of constraints that would be broken by choosing the value.
consistent :: Maybe v -> A i v Bool
consistent Nothing  = return False
consistent (Just x) = do
  view <- use agView
  uses agConstraints $ all isNothing . map (\c -> constraintCheck c view x)

-- | Bactrack.
-- Resolves nogood store, if succeeded sends new nogood (in BACKTRACK
-- message) to the agent with larger index among involved in conflict.
-- Otherwise sends STOP.
backtrack :: (Ord i, Eq v) => A i v ()
backtrack = do
  ngd <- uses agNoGoods resolveNoGoods
  case ngd of
    Nothing -> stopAgent -- no solution exists
    Just ng@NoGood{ngdRHS=(xj,_)} -> do
      agNoGoods %= filter (not . Map.member xj . ngdLHS)
      sendBacktrack xj ng
      agentUpdate xj Nothing
      checkAgentView

-- | Resolve nogood store.
-- Nothing means it cannot be resolved any further.
resolveNoGoods :: Ord i => [NoGood i v] -> Maybe (NoGood i v)
resolveNoGoods ngds = do
  let keys = map (Set.fromList . Map.keys . ngdLHS) ngds
      xs   = Set.unions keys
  if Set.null xs then
    Nothing
  else do
    let xj   = Set.findMax xs
        lhs  = Map.unions $ map ngdLHS ngds
        lhs' = Map.delete xj lhs
        rhs  = lhs Map.! xj
    return $ NoGood lhs' (xj, rhs)

