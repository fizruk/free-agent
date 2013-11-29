{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Algorithms.Termination
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Termination detection algorithms.
---------------------------------------------------------------------------
module Control.Agent.Free.Algorithms.Termination (
) where

import Control.Arrow
import Control.Agent.Free
import Control.Agent.Free.Interfaces.SendRecv
import Control.Monad.State
import Control.Monad.Trans.Free

data MessageSnapshot msg
  = MsgSnapshotRequest
  | MsgSnapshotMessage msg
  deriving (Show)

addTerminationBySnapshots :: (Ord i, Monad (t m)) => i -> [i] -> Agent (SendRecv i msg) t m a -> Agent (SendRecv i (Int, MessageSnapshot msg)) t m a
addTerminationBySnapshots myId neighbors = flip evalStateT (0, myId) . transform transF . hoistFreeT lift
  where
    transF (Send i msg next) = do
      time <- gets fst
      send i (time, MsgSnapshotMessage msg)
      next
    transF (Recv next) = do
      modify (succ *** const myId)
      x <- gets fst
      forM_ neighbors $ \i -> send i (x, MsgSnapshotRequest)
      (i, (t, msg)) <- recv
      case msg of
        MsgSnapshotMessage m -> do
          modify . first $ const (t + 1)
          next i m
        MsgSnapshotRequest -> do
          s <- get
          if (t, i) > s then do
            put (t, i)
            next undefined undefined  -- FIXME
          else
            next undefined undefined  -- FIXME: do nothing (repeat recv)

