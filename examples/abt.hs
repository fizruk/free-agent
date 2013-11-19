{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Trans.Free
import Control.Monad.State.Strict
import Control.Monad.Reader

import Control.Monad.Trans.Identity

import Control.Agent.Free

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

data AgentMsg
    = MsgOk     SenderId AgentValue
    | MsgNoGood SenderId NoGood
    | MsgStop
    deriving (Show)

data ABTF next
  = SendOk     AgentId AgentValue next
  | SendNoGood NoGood             next
  | SendStop                      next
  | GetMsg (AgentMsg -> next)
  | DumpState AgentState next
  | DumpString String next
  deriving (Functor)

-- =================================================================
-- XXX: boilerplate code
-- =================================================================

sendOk :: MonadFree ABTF m => AgentId -> AgentValue -> m ()
sendOk n x = liftF $ SendOk n x ()

sendNoGood :: MonadFree ABTF m => NoGood -> m ()
sendNoGood ngd = liftF $ SendNoGood ngd ()

sendStop :: MonadFree ABTF m => m ()
sendStop = liftF $ SendStop ()

getMsg :: MonadFree ABTF m => m AgentMsg
getMsg = liftF $ GetMsg id

dumpState :: MonadFree ABTF m => AgentState -> m ()
dumpState s = liftF $ DumpState s ()

dumpString :: MonadFree ABTF m => String -> m ()
dumpString s = liftF $ DumpString s ()

-- =================================================================

type AgentView = Map AgentId AgentValue
data NoGood = NoGood
  { ngdLHS :: AgentView
  , ngdRHS :: (AgentId, AgentValue)
  } deriving (Show)

data AgentState = AgentState
  { agentStop        :: Int
  , agentValue       :: Maybe (AgentValue)
  , agentView        :: AgentView
  , agentNoGoods     :: [NoGood]
  , agentAbove       :: Set AgentId
  , agentBelow       :: Set AgentId
  , agentConstraints :: [Constraint AgentId]
  , agentId          :: AgentId
  }

initAgentState :: AgentState
initAgentState = AgentState
  { agentStop        = 1
  , agentValue       = Nothing
  , agentView        = Map.empty
  , agentNoGoods     = []
  , agentAbove       = Set.empty
  , agentBelow       = Set.empty
  , agentConstraints = []
  , agentId          = error "agent ID is not set"
  }

type A a = forall m. Monad m => StateT AgentState (Agent' ABTF m) a

data AgentProps = AgentProps
  { agChan  :: TChan AgentMsg
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

data Constraint var = ConstraintNE var var deriving (Eq, Ord, Show)

exec :: AgentState -> A a -> ABT a
exec s m = execAgent' (join . interpretF) $ flip evalStateT s m

execABT :: Map AgentId AgentProps -> ABT a -> IO a
execABT agents = flip runReaderT initAbtState{abtAgents=agents} . runABT

interpretF :: ABTF a -> ABT a
interpretF (GetMsg next) = do
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
interpretF (SendNoGood ngd next) = do
  agId <- asks abtAgentId
  let dst = fst . ngdRHS $ ngd
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
interpretF (DumpState s next) = do
  agId <- asks abtAgentId
  liftIO $ do
    putStrLn $ show agId ++ ": =============================================="
    putStrLn $ show agId ++ ": Agent State Dump:"
    mapM_ (\(str, f) -> putStrLn $ show agId ++ ": " ++ str ++ " " ++ f s) $
      [ ("ID:         ", show . agentId)
      , ("Value:      ", show . agentValue)
      , ("Constrains: ", show . agentConstraints)
      , ("Above:      ", show . agentAbove)
      , ("Below:      ", show . agentBelow)
      , ("View:       ", show . agentView)
      , ("NoGoods:    ", show . agentNoGoods)
      ]
    putStrLn $ show agId ++ ": =============================================="
  return next
interpretF (DumpString s next) = do
  agId <- asks abtAgentId
  liftIO . putStrLn $ "Agent " ++ show agId ++ ": " ++ s
  return next

-- ----------------------------------------------------------------------

prg :: A (Maybe AgentValue)
prg = do
  checkAgentView
  msgLoop
  gets agentValue

msgLoop :: A ()
msgLoop = do
  stop <- gets agentStop
  unless (stop <= 0) $ do
    msg <- getMsg
    dumpString $ "Received msg: " ++ show msg
    get >>= dumpState
    case msg of
      MsgOk src val -> do
        agentUpdate src (Just val)
        checkAgentView
      MsgNoGood src ngd -> do
        resolveConflict src ngd
      MsgStop -> do
        modify $ \s -> s{agentStop = stop - 1}
    msgLoop

resolveConflict :: AgentId -> NoGood -> A ()
resolveConflict sender ngd = do
  agId <- gets agentId
  view <- gets agentView
  Just val <- gets agentValue
  let view' = fst $ Map.split agId view
      coh   = coherent (ngdLHS ngd) view'
      lhs'  = snd $ Map.split agId (ngdLHS ngd)
  if coh then do
    dumpString $ "nogood is coherent"
    mapM_ (\(k, v) -> agentUpdate k (Just v)) (Map.toList $ ngdLHS ngd)
    setValue Nothing
    newNoGood ngd
    checkAgentView
  else do
    dumpString $ "nogood is incoherent"
    unless (val /= snd (ngdRHS ngd)) $ do
      sendOk sender val

stopAgent :: A ()
stopAgent = do
  dumpString $ "no solution!"
  modify $ \s@AgentState{agentStop=stop} -> s{agentStop = stop - 1}
  sendStop

newNoGood :: NoGood -> A ()
newNoGood ngd = do
  dumpString $ "new nogood " ++ show ngd
  modify $ \s@AgentState{agentNoGoods=ngds} -> s{agentNoGoods=ngd:ngds}

agentUpdate :: AgentId -> Maybe AgentValue -> A ()
agentUpdate src val = do
  view <- gets agentView
  ngds <- gets agentNoGoods
  let view' =
        case val of
          Nothing -> Map.delete src view
          Just x  -> Map.insert src x view
  modify $ \s -> s{agentView = view'}
  modify $ \s -> s{agentNoGoods = filter (coherent view . ngdLHS) ngds}

coherent :: (Ord a, Eq b) => Map a b -> Map a b -> Bool
coherent ma mb = Map.null $ Map.differenceWith (\x y -> if x == y then Nothing else Just undefined) ma mb

checkAgentView :: A ()
checkAgentView = do
  val  <- gets agentValue
  view <- gets agentView
  (c, _) <- consistent val view
  unless c $ do
    val <- chooseValue
    setValue val
    case val of
      Nothing -> backtrack
      Just x  -> do
        ids <- gets $ Set.toList . agentAbove
        mapM_ (flip sendOk x) ids

setValue :: Maybe AgentValue -> A ()
setValue val = modify $ \s -> s{agentValue = val}

chooseValue :: A (Maybe AgentValue)
chooseValue = do
  agId <- gets agentId
  view <- gets agentView
  xs   <- filterM (checkVal view agId) [minBound..maxBound]
  return $ listToMaybe xs
  where
    checkVal view agId x = do
      ngds <- gets agentNoGoods
      let elm = or $ map (eliminates view (agId, x)) ngds
      if elm then
        return False
      else do
        (c, failed) <- consistent (Just x) view
        unless c $ do
          let ngd = constraintToNoGood view agId x (head failed)
          newNoGood ngd
        return c

constraintToNoGood :: AgentView -> AgentId -> AgentValue -> Constraint AgentId -> NoGood
constraintToNoGood view agId val c@(ConstraintNE x y)
  | agId == x = NoGood { ngdLHS = Map.fromList [ (y, view Map.! y) ], ngdRHS = (x, val) }
  | otherwise = constraintToNoGood view agId val (ConstraintNE y x)

eliminates :: AgentView -> (AgentId, AgentValue) -> NoGood -> Bool
eliminates view (agId, x) ngd =
  let view' = Map.insert agId x view
      lhs   = ngdLHS ngd
      rhs   = ngdRHS ngd
      ngd'  = uncurry Map.insert rhs lhs
      -- TODO: make this efficient
      diff  = Map.differenceWith (\x y -> if x == y then Nothing else Just undefined) view' ngd'
  in Map.null diff

consistent :: Maybe AgentValue -> AgentView -> A (Bool, [Constraint AgentId])
consistent Nothing _ = return (False, [])
consistent (Just x) view = do
    agId <- gets agentId
    let view' = Map.insert agId x view
    cs <- gets agentConstraints
    let failed = filter (not . checkConstraint view') cs
    return $ (null failed, failed)
    where
      checkConstraint view (ConstraintNE x y) = fromMaybe True $ do
        x' <- Map.lookup x view
        y' <- Map.lookup y view
        return $ x' /= y'

backtrack :: A ()
backtrack = do
  ngds <- gets agentNoGoods
  let ngd = solveNoGoods ngds
  dumpString $ "solved nogoods " ++ show ngds ++ " to " ++ show ngd
  case ngd of
    Nothing -> do
      stopAgent -- no solution exists
    Just ng@NoGood{ngdRHS=(xj,_)} -> do
      ngStore <- gets agentNoGoods
      modify $ \s -> s{agentNoGoods = filter (not . Map.member xj . ngdLHS) ngStore}
      sendNoGood ng
      agentUpdate xj Nothing
      get >>= dumpState
      checkAgentView

solveNoGoods :: [NoGood] -> Maybe NoGood
solveNoGoods ngds = do
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

-- ----------------------------------------------------------------------

createAgents :: [Constraint AgentId] -> Map AgentId AgentState
createAgents cs = Map.map (\cs -> initAgentState{agentConstraints=Set.toList $ Set.fromList cs}) . flip execState Map.empty $ do
    forM_ cs $ \c ->
      case c of
        ConstraintNE x y -> do
          forM_ [x, y] $ \z -> do
            modify $ Map.insertWith (++) x [c]
            modify $ Map.insertWith (++) y [c]

solve :: [Constraint AgentId] -> IO (Map AgentId (Maybe AgentValue))
solve cs = do
    let agents = Map.toList $ createAgents cs
        ids    = map fst agents
    xs <- forM agents $ \(agId, agState) -> do
      chan <- atomically $ newTChan
      let props    = AgentProps chan
          agState' = agState
            { agentAbove = Set.fromList $ filter (> agId) ids
            , agentBelow = Set.fromList $ filter (< agId) ids
            , agentId    = agId
            }
      return (agId, props, agState')
    let ps  = Map.fromList $ map (\(x, y, _) -> (x, y)) xs
        ags = map (\(x, _, z) -> (x, z)) xs
    execABT ps $ do
      waitAll <- forM ags $ \(agId, agState) -> do
        waitRes <- forkExec $ do
          liftIO . putStrLn $ "new agent started!"
          local (setAgentId agId) $ do
            exec agState prg
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
