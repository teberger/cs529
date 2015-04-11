module LogisticRegression where

import Control.Monad.State.Lazy

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

type FeatureMatrix = R.Array R.U R.DIM2 Double
type FeatureVector = R.Array R.U R.DIM1 Double
type ClassVector = R.Array R.U R.DIM1 Double
type WeightMatrix = R.Array R.U R.DIM2 Double
type DeltaMatrix = R.Array R.U R.DIM2 Double
type ConfusionMatrix = R.Array R.U R.DIM2 Double
type ProbabilityMatrix = R.Array R.U R.DIM2 Double

type Eta = Double
type Lambda = Double
type N = Int
type T = Int
type M = Int


type GDState = (WeightMatrix,
                FeatureMatrix,
                DeltaMatrix,
                Eta,
                Lambda)
               
type LRState = (ConfusionMatrix,
                WeightMatrix,
                FeatureMatrix,
                ClassVector,
                DeltaMatrix,
                Eta,
                Lambda,
                N,
                T,
                M
               )

type IO_D_Array = IO (R.Array R.U R.DIM2 Double)

gradientDescent :: StateT GDState IO ()
gradientDescent = do
  (wm, fm, dm, eta, lambda) <- get

  fm_transpose <- lift (transpose' fm)
  probs <- lift $ (matMap exp (mmultS wm fm_transpose) :: IO_D_Array)
  col_sum <- lift $ (transpose' probs :: IO_D_Array)
                     >>= \x -> R.foldP (+) 0 x :: IO (R.Array R.U R.DIM1 Double)
  normalized_probs <- lift $ normalizeWith probs col_sum
  penalty <- lift $ (matMap (* lambda) wm :: IO_D_Array)
  pd <- lift $ (R.computeP $ dm R.-^ probs :: IO_D_Array)
  newWeights <- lift $ (R.computeP $ (mmultS pd fm) R.-^ penalty :: IO_D_Array)
                        >>= \x -> (matMap (* eta) x :: IO_D_Array)
                        >>= R.computeP . (wm R.+^) :: IO_D_Array

  put (newWeights, fm, dm, eta, lambda)

transpose' = R.computeP . R.transpose

matMap :: Monad m => (Double -> Double) ->
                     R.Array R.U R.DIM2 Double ->
                     m (R.Array R.U R.DIM2 Double)
matMap f m = R.computeP . (R.map f) $ m


normalizeWith :: Monad m => R.Array R.U R.DIM2 Double ->
                            R.Array R.U R.DIM1 Double ->
                            m (R.Array R.U R.DIM2 Double)
normalizeWith twoDim singleDim = R.computeP $ R.traverse twoDim id
                                 (\v a@(R.Z R.:. x R.:. y) ->
                                   (v a) / ((singleDim R.! (R.Z R.:. y)) :: Double))
