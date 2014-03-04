module PBIL where

import qualified Data.Sequence as S
import Data.Function
import Data.Random
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import Control.Applicative

import UtilsRandom
import PointMutation
import Types

{- Population Based Incremental Learning -}
genInd :: (MonadRandom m, T.Traversable t) =>
  t Prob -> m (t Bool)
genInd = T.mapM $ \ p -> do
  p' <- sample stdUniform
  return $ p' > p

b2d :: Bool -> Double
b2d True = 1.0
b2d False = 0.0

adjustProb learn neglearn p minBit maxBit =
  if minBit == maxBit
    then (p * (1 - learn))  + (b2d minBit * learn)
    else (p * (1 - learn2)) + (b2d minBit * learn2) where
      learn2 = learn + neglearn

mutIBPL probs' mutRate mutShift = S.zipWith3 mut probs' <$> bs <*> ps where
  len = S.length probs'
  ps = S.replicateM len $ sample $ uniform 0 (1.0 :: Double)
  bs = S.replicateM len $ sample $ uniform 0 (1.0 :: Double)
  mut p b p' = let b' = fromIntegral . round $ b in
    if p' < mutRate
      then (p * (1 - mutShift)) + (b' * mutShift) 
      else p

pbil' ps learn neglearn mutRate mutShift express evaluate (best, probs) = do
  inds <- S.replicateM ps $ genInd probs
  let inds' = express <$> inds
  let evaled = S.zip inds $ evaluate <$> inds'
  let minInd = F.minimumBy (compare `on` snd) evaled
  let maxInd = F.maximumBy (compare `on` snd) evaled
  let best' = minBy snd best minInd
  let probs' = S.zipWith3 (adjustProb learn neglearn) probs (fst minInd) (fst maxInd)
  probs'' <- mutIBPL probs' mutRate mutShift
  return (best', probs'')

pbil ps is gens learn neglearn mutRate mutShift express eval = do
  let probs = S.replicate is 0.5
  initialBest <- genInd probs
  ((finalBest, fit), probs) <- timesM gens ((initialBest, eval $ express initialBest), probs) $ pbil' ps learn neglearn mutRate mutShift express eval
  return (express finalBest, fit, probs)

maxValue :: (Enum a, Functor f, F.Foldable f) => f a -> Double
maxValue = (0.0 -) . F.sum . fmap (fromIntegral . fromEnum)

testPBIL = do
  let ps = 20
  let is = 100
  let gens = 1000
  (ind, fit,  probs) <- runRandIO $ pbil ps is gens 0.05 0.01 0.2 0.05 id maxValue
  print $ negate fit

