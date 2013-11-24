{-# LANGUAGE FlexibleContexts #-}
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

import Control.Agent.Free
import Control.Agent.Free.Interfaces.SendRecv
import Control.Monad.State

data MessageSnapshot msg
  = MsgSnapshotRequest
  | MsgSnapshotResponse Snapshot
  | MsgSnapshotMessage msg

addTerminationBySnapshots :: (Ord i) => Agent (SendRecv i msg) t m a -> Agent (SendRecv i (Int, MessageSnapshot msg)) t m a
addTerminationBySnapshots = flip evalStateT (0, Nothing) $ transform transF
  where
    transF (Send i msg next) = do
      time <- gets fst
      send i (time, MsgSnapshotMessage msg)
      next
    transF (Recv next) = do
      modify (first succ)     -- increment local clock (FIXME: use max with my ID)
      (i, (n, msg)) <- recv
      modify (max (n, i))     -- "synchronize" clock
      -- TODO: ...


