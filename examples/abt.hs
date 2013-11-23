{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Trans.Free
import Control.Monad.State
import Control.Monad.Reader

import Control.Monad.Trans.Identity

import Control.Agent.Free
import Control.Agent.Free.Algorithms.ABT (Message(..), ABTKernelF(..), AgentState(..), A, NoGood(..), Constraint(..), abtKernel)
import qualified Control.Agent.Free.Algorithms.ABT as ABT

import Control.Concurrent.STM
import Control.Concurrent

import Data.Maybe (listToMaybe, fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Parallel (MonadParallel, MonadFork(..))

data Color = Red | Green | Blue deriving (Eq, Ord, Enum, Bounded, Show)

type AgentId = Int
type SenderId = AgentId
type AgentValue = Color

data AgentProps = AgentProps
  { agChan  :: TChan (Message AgentId AgentValue)
  }

data AbtState = AbtState
  { abtAgents   :: Map AgentId AgentProps
  , abtAgentId  :: AgentId
  }

initAbtState :: AbtState
initAbtState = AbtState
  { abtAgents   = Map.empty
  , abtAgentId  = error "called outside of agent code"
  }

newtype ABT a = ABT { runABT :: ReaderT AbtState IO a } deriving (Functor, Monad, MonadIO, MonadReader AbtState, MonadParallel, MonadFork)

data ConstraintNE var = ConstraintNE var var deriving (Eq, Ord, Show)

exec :: AgentState AgentId AgentValue -> A AgentId AgentValue a -> ABT a
exec s m = execAgent' (join . interpretF) $ evalStateT m s

execABT :: Map AgentId AgentProps -> ABT a -> IO a
execABT agents = flip runReaderT initAbtState{abtAgents=agents} . runABT

interpretF :: ABTKernelF AgentId AgentValue a -> ABT a
interpretF (Recv next) = do
  agId <- asks abtAgentId
  liftIO . putStrLn $ "DEBUG: Agent " ++ show agId ++ " is waiting for a message"
  chan <- asks $ agChan . (Map.! agId) . abtAgents
  msg  <- liftIO . atomically $ readTChan chan
  liftIO . putStrLn $ "DEBUG: Agent " ++ show agId ++ " reads a message"
  return (next msg)
interpretF (SendOk dst val next) = do
  agId <- asks abtAgentId
  chan <- asks $ agChan . (Map.! dst) . abtAgents
  let msg = MsgOk agId val
  liftIO . atomically $ writeTChan chan msg
  liftIO . putStrLn $ "DEBUG: Agent " ++ show agId ++ " sent MsgOk"
  return next
interpretF (SendBacktrack dst ngd next) = do
  agId <- asks abtAgentId
  chan <- asks $ agChan . (Map.! dst) . abtAgents
  let msg = MsgNoGood agId ngd
  liftIO . atomically $ writeTChan chan msg
  liftIO . putStrLn $ "DEBUG: Agent " ++ show agId ++ " sent MsgNoGood"
  return next
interpretF (SendStop next) = do
  cs <- asks $ map agChan . Map.elems . abtAgents
  forM_ cs $ \chan -> do
    liftIO . atomically $ writeTChan chan MsgStop
  return next

-- ----------------------------------------------------------------------

mkAgents :: [ConstraintNE AgentId] -> Map AgentId (AgentState AgentId AgentValue)
mkAgents cs = Map.mapWithKey mkState . Map.fromListWith (++) $ map leftC cs ++ map rightC cs
  where
    leftC (ConstraintNE x y) = (x, [(y, Constraint $ \view v -> do
        v' <- Map.lookup y view
        guard (v' /= v)
        return (NoGood (Map.singleton y v') (x, v))
      )] )
    rightC (ConstraintNE x y) = leftC (ConstraintNE y x)

    mkState k cs = ABT.initialAgentState
      { agConstraints  = map snd cs
      , agAbove        = filter (> k) $ map fst cs
      , agBelow        = filter (< k) $ map fst cs
      , agId           = k
      , agDomain       = [minBound..maxBound]
      }

solve :: [ConstraintNE AgentId] -> IO (Map AgentId (Maybe AgentValue))
solve cs = do
  let agents = Map.toList $ mkAgents cs
      ids    = map fst agents
  xs <- forM agents $ \(agId, agState) -> do
    chan <- atomically $ newTChan
    let props    = AgentProps chan
    return (agId, props, agState)
  let ps  = Map.fromList $ map (\(x, y, _) -> (x, y)) xs
      ags = map (\(x, _, z) -> (x, z)) xs
  execABT ps $ do
    waitAll <- forM ags $ \(agId, agState) -> do
      waitRes <- forkExec $ do
        liftIO . putStrLn $ "new agent started!"
        local (setAgentId agId) $ do
          exec agState abtKernel
      return (agId, waitRes)
    res <- forM waitAll $ \(agId, waitRes) -> do
      x <- waitRes
      return (agId, x)
    return $ Map.fromList res
  where
    setAgentId agId s = s{abtAgentId = agId}

main :: IO ()
main = do
  mapM_ print constraints
  res <- solve constraints
  forM_ (Map.toList res) $ \(k, v) -> do
    putStrLn $ show k ++ " -> " ++ show v
  where
    constraints = 
      [ ConstraintNE 1 2
      , ConstraintNE 1 3
      ]
