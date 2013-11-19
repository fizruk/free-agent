{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Agent.Free
import Control.Applicative
import Control.Monad.Free.TH
import Control.Monad.Trans.Free
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = m >>= maybe (untilJust m) return

newtype AlgoMsg = AlgoMsg { unAlgoMsg :: Int } deriving (Eq, Show, Num)

data AlgoF x
  = AlgoSend AlgoMsg x
  | AlgoRecv (AlgoMsg -> x)
  deriving (Functor)
makeFree ''AlgoF

type AlgoParams = Int
type Algo a = forall m. Monad m => Agent' AlgoF m a

simpleAlgo :: AlgoParams -> Algo Int
simpleAlgo n = flip evalStateT n $ do
  algoSend (AlgoMsg n)
  AlgoMsg m <- algoRecv
  return (n + m)

data BotMsg
  = BotMsgAlgo AlgoMsg
  deriving (Show)
makePrisms ''BotMsg

data BotF msg x
  = BotSend msg x
  | BotRecv (msg -> x)
  | BotOutput String x
  deriving (Functor)
makeFree ''BotF

type Bot msg a = forall m. Monad m => Agent' (BotF msg) m a

execAlgoInBot :: Prism' msg AlgoMsg -> Algo a -> Bot msg a
execAlgoInBot p algo = execAgent' execF algo
  where
    execF (AlgoSend msg next) = do
      botSend (review p msg)
      next
    execF (AlgoRecv next) = do
      msg <- untilJust $ preview p <$> botRecv
      next msg

bot :: Bot BotMsg ()
bot = flip evalStateT "John" $ do
  n <- lift $ execAlgoInBot _BotMsgAlgo (simpleAlgo 10)
  name <- get
  botOutput $ name ++ ": " ++ show n

execBot :: (MonadIO m, MonadState [msg] m, Show msg) => Bot msg a -> m a
execBot bot = execAgent' execF bot
  where
    execF (BotSend msg next) = do
      liftIO . putStrLn $ "sending: " ++ show msg
      next
    execF (BotRecv next) = do
      msg <- gets head
      modify tail
      next msg
    execF (BotOutput s next) = do
      liftIO $ putStrLn s
      next

main :: IO ()
main = do
  let msgs = map (BotMsgAlgo . AlgoMsg) [1..]
  evalStateT (execBot bot) msgs

