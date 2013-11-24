{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Agent.Free.Interfaces.SendRecv
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- A simple send/receive interface.
---------------------------------------------------------------------------
module Control.Agent.Free.Interfaces.SendRecv (
  -- * Interface
    SendRecv(..)
  , send
  , recv
) where

import Control.Monad.Free.Class

-- | Abstract interface for send/receive communication.
-- @i@ represents sender/receiver ID and @msg@ is a type of messages.
data SendRecv i msg next
  = -- | Send a message. See also 'send'.
    Send i msg next
    -- | Receive a message. See also 'recv'.
  | Recv (i -> msg -> next)
  deriving (Functor)

-- | Send a 'Message'.
send :: MonadFree (SendRecv i msg) m => i -> msg -> m ()
send i msg = liftF $ Send i msg ()

-- | Receive a 'Message'.
recv :: MonadFree (SendRecv i msg) m => m (i, msg)
recv = liftF $ Recv (,)

