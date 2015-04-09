module LogisticRegression where

import Control.Monad.State.Lazy

import qualified Data.Array.Repa as R
import Parsing

type FeatureMatrix = R.Array R.U R.DIM2 Double
type FeatureVector = R.Array R.U R.DIM1 Double
type ClassVector = R.Array R.U R.DIM1 Double
type WeightMatrix = R.Array R.U R.DIM2 Double
type DeltaMatrix = R.Array R.U R.DIM2 Double
type ConfusionMatrix = R.Array R.U R.DIM2 Double
type ProbabilityMatrix = R.Array R.U R.DIM2 Double

--type LRState = [Instance]

type Eta = Double
type Lambda = Double
type N = Int
type T = Int
type M = Int

type LRState = (ConfusionMatrix,
                WeightMatrix,
                FeatureMatrix,
                ClassVector,
                DeltaMatrix,
                Eta,
                Lambda,
                N,
                T,
                M,
                ProbabilityMatrix
               )

logisticRegression :: StateT LRState IO LRState
logisticRegression = do
  (cm, wm, fm, cv, dm, eta, lambda, n, t, m, probs) <- get

  return undefined
--  return ()



