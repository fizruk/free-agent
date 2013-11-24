{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Environments.STM
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Common interfaces.
---------------------------------------------------------------------------
module Control.Agent.Free.Environments.STM (
  -- * SendRecv
    SendRecvParams(..)
  , initSendRecvParams
  , interpretSendRecv
) where

import Control.Concurrent.STM
import Control.Agent.Free.Interfaces
import Control.Monad.Reader.Class
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map

data SendRecvParams i msg = SendRecvParams
  { sendRecvChans :: Map i (TChan (i, msg))
  , sendRecvId    :: i
  }

initSendRecvParams :: i -> SendRecvParams i msg
initSendRecvParams = SendRecvParams Map.empty

interpretSendRecv :: (Ord i, MonadReader (SendRecvParams i msg) m, MonadIO m) => SendRecv i msg a -> m a
interpretSendRecv (Send i msg next) = do
  myId <- asks sendRecvId
  chan <- asks $ (Map.! i) . sendRecvChans
  liftIO . atomically $ writeTChan chan (myId, msg)
  return next
interpretSendRecv (Recv next) = do
  myId <- asks sendRecvId
  chan <- asks $ (Map.! myId) . sendRecvChans
  (i, msg) <- liftIO . atomically $ readTChan chan
  return $ next i msg

