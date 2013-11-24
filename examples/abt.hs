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
import Control.Agent.Free.Algorithms.ABT
import Control.Agent.Free.Environments.STM

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

data ConstraintNE var = ConstraintNE var var deriving (Eq, Ord, Show)

exec :: (Ord i, MonadReader (SendRecvParams i (Message i v)) m, MonadIO m) => AgentState i v -> A i v a -> m a
exec s m = execAgent' (join . interpretF) $ evalStateT m s

interpretF :: (Ord i, MonadReader (SendRecvParams i msg) m, MonadIO m) => SendRecv i msg a -> m a
interpretF f = do
  myId <- asks sendRecvId
  -- liftIO . putStrLn $ "Agent " ++ show myId ++ ": " ++ show (void f)
  interpretSendRecv f

-- ----------------------------------------------------------------------

mkAgents :: [ConstraintNE AgentId] -> Map AgentId (AgentState AgentId AgentValue)
mkAgents cs = Map.mapWithKey mkState . Map.fromListWith (++) $ map leftC cs ++ map rightC cs
  where
    leftC (ConstraintNE x y) = (x, [(y, Constraint $ \view v -> do
        v' <- Map.lookup y view
        guard (v' == v)
        return (NoGood (Map.singleton y v') (x, v))
      )] )
    rightC (ConstraintNE x y) = leftC (ConstraintNE y x)

    mkState k cs = initialAgentState
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
    return (agId, chan, agState)
  let ps  = Map.fromList $ map (\(x, y, _) -> (x, y)) xs
      ags = map (\(x, _, z) -> (x, z)) xs
  flip runReaderT (SendRecvParams ps undefined) $ do
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
    setAgentId agId s = s{sendRecvId = agId}

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
      , ConstraintNE 1 4
      , ConstraintNE 2 3
      , ConstraintNE 2 4
      , ConstraintNE 3 4
      ]
