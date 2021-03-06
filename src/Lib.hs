{-# language PackageImports #-}
module Lib where

import qualified "fgl" Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified "mwc-probability" System.Random.MWC.Probability as P

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)

-- import System.Random

{-- |
* Start and end at node 0
* Maximize tour profit (sum of node values)
* Edge costs represent travel times (constraint on max.travel time)
--}



data St r = St { reward :: r}


data MDP m s r t =
  MDP {
    futureStates :: s -> [s]
  , actionChoice :: [s] -> P.Prob m s
  , evalState :: s -> r       -- ^ given current state, return value
  , evalTrans :: s -> Maybe t -- ^ given a future state, return cost of transition
  -- , state :: s
  -- , rewardTot :: r
  -- , constraintTot :: t
        }

-- updateAgent gr = do
--   (Agent s r) <- get         -- current state
--   let ss' = G.neighbors gr s -- available actions
  
  -- put $ Agent s' 

-- evalState = G.lab
-- evalFutureStates gr i 


-- * Solvers

{-- | Randomized Heuristic 1
1) Start from node 1 -> i : cost = 0, value = 0
2) pick random node in neighborhood -> i'
** value = value + value(i')
** cost = cost + cost(i, i')
** remove current node i from future neighbors list
** GOTO 2)
--}


-- NB : we must track provenance: what node are we arriving from?

sampleTransition
  :: Monad m =>
     P.Prob m a1
     -> (a -> a1 -> a) -> P.Gen (PrimState m) -> StateT a m ()
sampleTransition prob f gen = do
  x <- get
  w <- lift $ P.sample prob gen
  let z = f x w
  put z
  -- return z

sampleGraphTransition1
  :: (PrimMonad m, G.Graph gr) =>
     gr a1 b
     -> G.Node
     -> (a -> G.Node -> a)
     -> P.Gen (PrimState m)
     -> StateT a m ()
sampleGraphTransition1 gr i =
  sampleTransition (P.discreteUniform $ G.neighbors gr i)



pickNeighborNode :: (PrimMonad m, G.Graph gr) => gr a b -> G.Node -> P.Prob m G.Node
pickNeighborNode gr i = 
  P.discreteUniform (G.neighbors gr i)





{-- fgl queries:
lab : node label
neighbors : neighboring nodes

lneighbors : edges (label, second node)
out : edges (this node, second node, label)

--}





-- * Datasets

-- | Easy instance: 6 nodes. Max. cost = 2 hours (= 120)
g0 :: G.Gr (St Int) Int
g0 = G.mkGraph (zip [1..6] nodeLabels) edgeCosts where
  edgeCosts = [(1,2,25),(1,3,32),(1,4,43),(1,5,35),(1,6,21),(2,3,23),(2,4,28),(2,5,41),(2,6,24),(3,4,16),(3,5,24),(3,6,36),(4,5,13),(4,6,34),(5,6,27)]
  nodeLabels = [St 0, St 13, St 12, St 19, St 16, St 15]






-- * Utils

withIOGen :: (P.Gen RealWorld -> IO a) -> IO a
withIOGen = P.withSystemRandom . P.asGenIO



-- misc

rands :: PrimMonad m => P.Gen (PrimState m) -> (Int, Int) -> Int -> m [Int]
rands g (n1,n2) m = 
  replicateM m $ P.sample (P.discreteUniform [n1..n2]) g

rands' g (n1,n2) m = 
  replicateM m  (P.discreteUniform [n1..n2])
