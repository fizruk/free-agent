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

  -- | Asynchronous backtracking (ABT) algorithms provide an adaptation of
  -- local backtracking for distributed environment. With free monad
  -- representation of agent's program it becomes possible to provide such
  -- algorithms as a library code.
  --
  -- Each ABT algorithms operate using following objects:
  --
  -- * agent's view:
  --    * current value choosed by an agent,
  --    * values other agents have (or an agent thinks they have),
  --    * contraints which bind an agent;
  -- * nogood store (known conflicts with other agents).
  --
  -- In order to perform an ABT algorithm an environment should provide
  -- 'SendRecv' interface for 'Message's.
  --
  -- /NOTE/: ABT algorithm does not stop when solution is found.
  -- So when you run ABT algorithm it may
  --
  -- * stop (meaning there's no solution) or
  -- * became inactive (with no messages floating around, meaning solution
  -- has been found).
  --
  -- You need to use a termination detection algorithm to detect whether
  -- solution has been found already.

  module Control.Agent.Free.Interfaces.SendRecv
  -- * Algorithms
  , A
  , abtKernel
  -- * ABT Kernel API
  , ABTKernelF
  , sendOk
  , sendBacktrack
  , sendStop
  -- * Used data structures
  , Constraint(..)
  , Message(..)
  , NoGood(..)
  , AgentView
  , AgentState(..)
  , initialAgentState
  -- * Example
  -- $example
) where

import Control.Arrow
import Control.Agent.Free
import Control.Agent.Free.Interfaces.SendRecv
import Control.Monad
import Control.Monad.Free.Class
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
    MsgOk v
    -- | BACKTRACK message is sent to lower agent to force it rechoose its value.
  | MsgBacktrack (NoGood i v)
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
type ABTKernelF i v = SendRecv i (Message i v)

-- | Send OK? message. Requires address of another agent and a chosen value.
sendOk :: MonadFree (ABTKernelF i v) m => i -> v -> m ()
sendOk i v = send i (MsgOk v)

-- | Send BACKTRACK message. Requires address of another agent and resolved nogood store.
sendBacktrack :: MonadFree (ABTKernelF i v) m => i -> NoGood i v -> m ()
sendBacktrack i ngd = send i (MsgBacktrack ngd)

-- | Send STOP message to the *system*. All agents in the system will receive this message.
sendStop :: MonadFree (ABTKernelF i v) m => i -> m ()
sendStop i = send i MsgStop

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
    (src, msg) <- recv
    case msg of
      MsgOk val -> do
        agentUpdate src (Just val)
        checkAgentView
      MsgBacktrack ngd -> do
        resolveConflict src ngd
      MsgStop -> do
        modify (\s -> s{ agStop = True, agValue = Nothing })
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
  modify (\s -> s{ agStop = True, agValue = Nothing })
  (as, bs) <- gets (agAbove &&& agBelow)
  mapM_ sendStop (as ++ bs)

-- | Update agent's view.
agentUpdate :: (Ord i, Eq v) => i -> Maybe v -> A i v ()
agentUpdate src val = do
  modify (\s@AgentState{agView=view} -> s{ agView = Map.alter (const val) src view })
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
        mapM_ (`sendOk` x) ids

-- | Try to choose a value w.r.t. nogood store, constraints
-- and current view.
chooseValue :: Eq v => A i v (Maybe v)
chooseValue = do
  -- eliminate values by nogood store
  xs   <- gets agDomain >>= eliminateNoGoods -- FIXME: too strict
  -- check the rest for consistency with constraints
  view <- gets agView
  cs   <- gets $ map (`constraintCheck` view) . agConstraints
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

-- BEGIN GraphColor.lhs
{- $example
This is literate Haskell! To run the example, open the source and copy
this comment block into a new file with '.lhs' extension. Compiling to an executable
file with the @-O2@ optimization level and @-threaded -rtsopts@ options is recomended.

For example: @ghc -o 'graph-color' -O2 -threaded -rtsopts GraphColor.lhs ; ./graph-color@

@ \{\-\# LANGUAGE PackageImports \#\-\} @

> {-# LANGUAGE PackageImports #-}

> import Control.Arrow
> import Control.Monad.Trans.Iter
> import "mtl" Control.Monad.Reader
> import "mtl" Control.Monad.List
> import "mtl" Control.Monad.Identity
> import Control.Monad.IO.Class
> import Data.Complex
> import Graphics.HGL (runGraphics, Window, withPen,
>                      line, RGB (RGB), RedrawMode (Unbuffered, DoubleBuffered), openWindowEx,
>                      drawInWindow, mkPen, Style (Solid))

Some fractals can be defined by infinite sequences of complex numbers. For example,
to render the <https://en.wikipedia.org/wiki/Mandelbrot_set Mandelbrot set>,
the following sequence is generated for each point @c@ in the complex plane:

  z₀ = c

  z₁ = z₀² + c

  z₂ = z₁² + c

  …

If, after some iterations, |z_i| ≥ 2, the point is not in the set. We
can compute if a point is not in the Mandelbrot set this way:

@
 escaped :: Complex Double -> Int
 escaped c = loop 0 0 where
   loop z n = if (magnitude z) >= 2 then n
                                    else loop (z*z + c) (n+1)
@

If @c@ is not in the Mandelbrot set, we get the number of iterations required to
prove that fact. But, if @c@ is in the mandelbrot set, 'escaped' will
run forever.

We can use the 'Iter' monad to delimit this effect. By applying
'delay' before the recursive call, we decompose the computation into
terminating steps.

> escaped :: Complex Double -> Iter Int
> escaped c = loop 0 0 where
>   loop z n = if (magnitude z) >= 2 then return n
>                                    else delay $ loop (z*z + c) (n+1)
>

If we draw each point on a canvas after it escapes, we can get a _negative_
image of the Mandelbrot set. Drawing pixels is a side-effect, so it
should happen inside the IO monad. Also, we want to have an
environment to store the size of the canvas, and the target window.

By using 'IterT', we can add all these behaviours to our non-terminating
computation.

> data Canvas = Canvas { width :: Int, height :: Int, window :: Window }
>
> type FractalM a = IterT (ReaderT Canvas IO) a

Any simple, non-terminating computation can be lifted into a richer environment.

> escaped' :: Complex Double -> IterT (ReaderT Canvas IO) Int
> escaped' = liftIter . escaped

Then, to draw a point, we can just retrieve the number of iterations until it
finishes, and draw it. The color will depend on the number of iterations.

> mandelbrotPoint :: (Int, Int) -> FractalM ()
> mandelbrotPoint p = do
>   c <- scale p
>   n <- escaped' c
>   let color =  if (even n) then RGB   0   0 255 -- Blue
>                            else RGB   0   0 127 -- Darker blue
>   drawPoint color p

The pixels on the screen don't match the region in the complex plane where the
fractal is; we need to map them first. The region we are interested in is
Im z = [-1,1], Re z = [-2,1].

> scale :: (Int, Int) -> FractalM (Complex Double)
> scale (xi,yi) = do
>   (w,h) <- asks $ (fromIntegral . width) &&& (fromIntegral . height)
>   let (x,y) = (fromIntegral xi, fromIntegral yi)
>   let im = (-y + h / 2     ) / (h/2)
>   let re = ( x - w * 2 / 3 ) / (h/2)
>   return $ re :+ im

Drawing a point is equivalent to drawing a line of length one.

> drawPoint :: RGB -> (Int,Int) -> FractalM ()
> drawPoint color p@(x,y) = do
>   w <- asks window
>   let point = line (x,y) (x+1, y+1)
>   liftIO $ drawInWindow w $ mkPen Solid 1 color (flip withPen point)


We may want to draw more than one point. However, if we just sequence the computations
monadically, the first point that is not a member of the set will block the whole
process. We need advance all the points at the same pace.

> iterForM_ :: (Monad m) => [a] -> (a -> IterT m ()) -> IterT m ()
> iterForM_ as f = exhaust $ map f as
>   where
>     exhaust :: (Monad m) => [IterT m ()] -> IterT m ()
>     exhaust [] = return ()
>     exhaust xs = IterT $ do
>       l <- mapM runIterT xs
>       return $ Right $ exhaust $ l >>= (either (const []) (:[]))

With this combinator in place, drawing the complete set is just drawing each
of the possible points:

> drawMandelbrot :: FractalM ()
> drawMandelbrot = do
>   (w,h) <- asks $ width &&& height
>   let ps = [(x,y) | x <- [0 .. (w-1)], y <- [0 .. (h-1)]]
>   iterForM_ ps mandelbrotPoint

To run this computation, we can just use @retract@, which will run indefinitely:

> runFractalM :: Canvas -> FractalM a -> IO a
> runFractalM canvas  = flip runReaderT canvas . retract

Or, we can trade non-termination for getting an incomplete result,
by cutting off after a certain number of steps.

> runFractalM' :: Integer -> Canvas -> FractalM a -> IO (Maybe a)
> runFractalM' n canvas  = flip runReaderT canvas . retract . cutoff n

Thanks to the 'IterT' transformer, we can separate timeout concerns from
computational concerns.

> main :: IO ()
> main = do
>   let windowWidth = 800
>   let windowHeight = 480
>   runGraphics $ do
>     w <- openWindowEx "Mandelbrot" Nothing (windowWidth, windowHeight) DoubleBuffered (Just 1)
>     let canvas = Canvas windowWidth windowHeight w
>     runFractalM' 100 canvas drawMandelbrot
>     putStrLn $ "Fin"

-}
-- END GraphColor.lhs
