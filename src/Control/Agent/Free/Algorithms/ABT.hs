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
-- Family of ABT algorithms. Based on «The Asynchronous Backtracking Family» 
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.60.4716>.
---------------------------------------------------------------------------
module Control.Agent.Free.Algorithms.ABT (
  -- * Algorithms
    A
  , abtKernel
  -- * ABT Kernel API
  , ABTKernelF(..)
  , sendOk
  , sendBacktrack
  , sendStop
  , recv
  -- * Used data structures
  , Message(..)
  , NoGood(..)
  , AgentState(..)
  , initialAgentState
) where

import Control.Arrow
import Control.Agent.Free
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.State

import qualified Data.Foldable as F

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

-- | ABT Kernel messages.
data Message i v
  = -- | OK? message is sent to higher agents so they could check it.
    MsgOk i v
    -- | BACKTRACK message is sent to lower agent to force it rechoose its value.
  | MsgNoGood i (NoGood i v)
    -- | STOP message is sent when it is know that a problem has no solution.
  | MsgStop
  deriving (Show)

-- | 'NoGood' is a witness of a previous conflict. It is of the form
-- @x_i1 = v1 && ... && x_iN = vN  => x_j /= v@, where @x_j@ refers to the agent holding
-- this nogood in its nogood store.
data NoGood i v = NoGood
  { ngdLHS :: Map i v   -- ^ Left hand side of a nogood.
  , ngdRHS :: (i, v)    -- ^ Right hand side of a nogood.
  }
  deriving (Show)

-- | Abstract interface used by ABT Kernel algorithm.
data ABTKernelF i v next
  = -- | Send OK? message. See also 'sendOk'.
    SendOk i v next
    -- | Send BACKTRACK message. See also 'sendBacktrack'.
  | SendBacktrack i (NoGood i v) next
    -- | Send STOP to the *system*. See also 'sendStop'.
  | SendStop next
    -- | Receive a message. See also 'recv'.
  | Recv (Message i v -> next)
  deriving (Functor)

-- | Send OK? message. Requires address of another agent and a chosen value.
sendOk :: MonadFree (ABTKernelF i v) m => i -> v -> m ()
sendOk i v = liftF $ SendOk i v ()

-- | Send BACKTRACK message. Requires address of another agent and resolved nogood store.
sendBacktrack :: MonadFree (ABTKernelF i v) m => i -> (NoGood i v) -> m ()
sendBacktrack i ngd = liftF $ SendBacktrack i ngd ()

-- | Send STOP message to the *system*. All agents in the system will receive this message.
sendStop :: MonadFree (ABTKernelF i v) m => m ()
sendStop = liftF $ SendStop ()

-- | Receive message.
recv :: MonadFree (ABTKernelF i v) m => m (Message i v)
recv = liftF $ Recv id

-- | Agent view is just a 'Map' from agents' adresses to agents' values.
type AgentView i v = Map i v

-- | A general constraint. @'Constraint' i v@ is agent-oriented and is just a wrapper for
-- the function of type @'AgentView' i v -> v -> Maybe ('NoGood' i v)@. The latter checks
-- whether constraint would hold given agent's view and a value it's going to choose. If constraint
-- wouldn't hold, the function returns @Just ngd@ (it generates 'NoGood'), otherwise it returns 'Nothing'.
newtype Constraint i v = Constraint
  { constraintCheck  :: AgentView i v -> v -> Maybe (NoGood i v) }

-- | State of an agent for ABT Kernel algorithm.
data AgentState i v = AgentState
  { agStop        :: Bool             -- ^ Flag informing whether algorithm should stop.
  , agValue       :: Maybe v          -- ^ Currently chosen value.
  , agDomain      :: [v]              -- ^ Domain of agent's value.
  , agId          :: i                -- ^ ID (address) of an agent.
  , agView        :: AgentView i v    -- ^ Current agent's view.
  , agAbove       :: [i]              -- ^ IDs of higher agents.
  , agBelow       :: [i]              -- ^ IDs of lower agents.
  , agConstraints :: [Constraint i v] -- ^ Constraints involving an agent.
  , agNoGoods     :: [NoGood i v]     -- ^ Agent's nogood store.
  }

-- | Initial agent's state.
initialAgentState :: AgentState i v
initialAgentState = AgentState
  { agStop        = False
  , agValue       = Nothing
  , agDomain      = []
  , agId          = undefined
  , agView        = Map.empty
  , agAbove       = []
  , agBelow       = []
  , agConstraints = []
  , agNoGoods     = []
  }

-- | A useful alias for a monad in which ABT algorithms run.
type A i v a = forall m. Monad m => StateT (AgentState i v) (Agent' (ABTKernelF i v) m) a

updateNoGoods :: MonadState (AgentState i v) m => ([NoGood i v] -> [NoGood i v]) -> m ()
updateNoGoods f = modify (\s@AgentState{agNoGoods=ngds} -> s{ agNoGoods = f ngds })

-- | ABT Kernel algorithm.
abtKernel :: (Ord i, Eq v) => A i v (Maybe v)
abtKernel = do
  checkAgentView
  msgLoop
  gets agValue

msgLoop :: (Ord i, Eq v) => A i v ()
msgLoop = do
  stop <- gets agStop
  unless stop $ do
    msg <- recv
    case msg of
      MsgOk src val -> do
        agentUpdate src (Just val)
        checkAgentView
      MsgNoGood src ngd -> do
        resolveConflict src ngd
      MsgStop -> do
        modify (\s -> s{ agStop = False })
    msgLoop

-- | Resolve conflict by
resolveConflict :: (Ord i, Eq v) => i -> NoGood i v -> A i v ()
resolveConflict sender ngd = do
  view <- gets agView
  if coherent (ngdLHS ngd) view then do
    updateNoGoods (ngd:)
    modify (\s -> s{ agValue = Nothing })
    checkAgentView
  else do
    val <- gets agValue
    let val' = snd . ngdRHS $ ngd
    when (val == Just val') $ do
      sendOk sender val'

-- | Stop and send STOP message.
-- The solution does not exist.
stopAgent :: A i v ()
stopAgent = do
  modify (\s -> s{ agStop = False })
  sendStop

-- | Update agent's view.
agentUpdate :: (Ord i, Eq v) => i -> Maybe v -> A i v ()
agentUpdate src val = do
  modify (\s@AgentState{agView=view} -> s{ agView = Map.update (const val) src view })
  view <- gets agView
  updateNoGoods $ filter (coherent view . ngdLHS)

-- | Check whether NoGood is coherent with agent's view (i.e. whether
-- nogood's LHS does not contain old information about values).
coherent :: (Ord a, Eq b) => Map a b -> Map a b -> Bool
coherent ma mb = F.and $ Map.intersectionWith (==) ma mb

-- | Recheck whether current view is consistent with agent's value.
-- If not - try rechoose value.
checkAgentView :: (Ord i, Eq v) => A i v ()
checkAgentView = do
  c <- gets agValue >>= consistent
  unless c $ do
    val <- chooseValue
    modify (\s -> s{ agValue = val })
    case val of
      -- unable to choose a value
      Nothing -> do
        backtrack
      -- value chosen
      Just x  -> do
        ids <- gets agAbove
        mapM_ (flip sendOk x) ids

-- | Try to choose a value w.r.t. nogood store, constraints
-- and current view.
chooseValue :: Eq v => A i v (Maybe v)
chooseValue = do
  -- eliminate values by nogood store
  xs   <- gets agDomain >>= eliminateNoGoods -- FIXME: too strict
  -- check the rest for consistency with constraints
  view <- gets agView
  cs   <- gets $ map (flip constraintCheck view) . agConstraints
  let (ngds, v) = choose (msum . zipWith ($) cs . repeat) xs
  updateNoGoods (ngds ++)
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
  ys <- gets $ map (snd . ngdRHS) . agNoGoods
  return $ xs \\ nub ys

-- | Check value for consistency with current view and constraints.
-- Returns the list of constraints that would be broken by choosing the value.
consistent :: Maybe v -> A i v Bool
consistent Nothing  = return False
consistent (Just x) = do
  view <- gets agView
  gets $ all isNothing . map (\c -> constraintCheck c view x) . agConstraints

-- | Bactrack.
-- Resolves nogood store, if succeeded sends new nogood (in BACKTRACK
-- message) to the agent with larger index among involved in conflict.
-- Otherwise sends STOP.
backtrack :: (Ord i, Eq v) => A i v ()
backtrack = do
  ngd <- gets $ resolveNoGoods . agNoGoods
  case ngd of
    Nothing -> stopAgent -- no solution exists
    Just ng@NoGood{ngdRHS=(xj,_)} -> do
      updateNoGoods $ filter (not . Map.member xj . ngdLHS)
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

