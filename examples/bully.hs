{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Arrow

import Control.Monad.Trans.Free

import Control.Monad.Identity
import Control.Agent.Free
import Control.Agent.Free.Interfaces.SendRecv

import Control.Monad.Reader
import Control.Monad.State

import Data.List (partition)

type Free f = FreeT f Identity

data Message = Ack | Down | Halt | Ldr

data Status = Norm | Elec1 | Elec2 | Wait

type A i a = StateT (Status, Int) (ReaderT (i, [i], [i]) (Free (SendRecv i Message))) a

runBully :: (i, [i], [i]) -> A i i -> Free (SendRecv i Message) i
runBully e m = runReaderT (evalStateT m (Norm, 0)) e

bullyLoop :: Ord i => A i i
bullyLoop = do
  (myId, lowerIds, higherIds) <- ask
  (status, n) <- get
  (src, msg) <- recv
  case (status, msg) of
    (Wait,  Ldr)  -> return src
    (Wait,  Down) -> initiate
    (Elec2, Ack)  -> do
      modify $ second (+ 1)
      if n + 1 == length higherIds
        then becomeLeader
        else bullyLoop
    (Elec1, Down) -> do
      modify $ second (+ 1)
      when (n + 1 == length lowerIds) $ put (Elec2, 0)
      bullyLoop
    (_, Halt)     -> do
      send src Ack
      put (Wait, 0)
      bullyLoop
    (_, Ack)      -> initiate
    (_, _)        -> bullyLoop

becomeLeader :: Ord i => A i i
becomeLeader = do
  (myId, _, higherIds) <- ask
  mapM_ (flip send Ldr) higherIds
  return myId

initiate :: Ord i => A i i
initiate = do
  (myId, lowerIds, _) <- ask
  put (Elec1, 0)
  mapM_ (flip send Ack) lowerIds
  msgs <- replicateM (length lowerIds) recv
  bullyLoop

main :: IO ()
main = undefined
