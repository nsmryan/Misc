module Crossover where

import qualified Data.Sequence as S
import Data.Random
import Data.List
import Data.Conduit

import Control.Monad.IO.Class

import Control.Monad

import Types
import UtilsRandom

{- Crossover -}
crossPair crossPoint (a, b) = 
  (top S.>< bottom', top' S.>< bottom) where
    (top,  bottom)  = S.splitAt crossPoint a
    (top', bottom') = S.splitAt crossPoint b
      
unpair as = (fmap fst as, fmap snd as)

crossoverPure :: 
  [Int] -> [Int] -> Pop (Ind a) -> Pop (Ind a)
crossoverPure crossPoints crossPoint population = let
  (top, bottom) = S.splitAt midPoint population
  midPoint = S.length population `div` 2
  pairs = S.zip top bottom
  crossedPairs = applyOn 1 (map (const . crossPair) crossPoints) crossPoints pairs
  (firstHalf, secondHalf) = unpair crossedPairs
    in firstHalf S.>< secondHalf

crossover :: (MonadRandom m, Functor m) =>
  Prob -> Pop (Ind a) -> m (Pop (Ind a))
crossover p population = do
  let totalLength = S.length population `div` 2
      len = S.length $ population `S.index` 0
  indices <- generateIndices totalLength p
  crossPoints <- replicateM (length indices) $ fromRange len
  return $ crossoverPure indices crossPoints population


{- Multipoint crossover -}
foldInHalf s = S.zip top bottom where
  (top, bottom) = S.splitAt midPoint s
  midPoint = S.length s `div` 2

exchange as bs = exchange' S.empty S.empty as bs

exchange' rest rest' [] [] = (rest, rest')
exchange' rest rest' (a:as) (b:bs) = exchange' (rest S.>< a) (rest' S.>< b) bs as

splits [] as = [as]
splits (p:points) as = top : splits points bottom where
  (top, bottom) = S.splitAt p as

multicrossPair ixs (a, b) =
  let len = S.length a
      points = sort ixs
  in exchange (splits points a) (splits points b)

multipointCrossoverPure ::
  [Int] -> [[Int]] -> Pop (Ind a) -> Pop (Ind a)
multipointCrossoverPure crossIndices crossPoints pop = let
  pairs = foldInHalf pop
  result = applyOn 1 (map (const . multicrossPair) crossPoints) crossIndices pairs
  (firstHalf, secondHalf) = unpair result
  in firstHalf S.>< secondHalf

multipointCrossover :: (Functor m, MonadRandom m) =>
  Prob -> -- probability of crossover
  Int -> --number of cross points
  Pop (Ind a) -> --population to crossover
  m (Pop (Ind a))
multipointCrossover pc points pop = do
  let indLen = S.length $ pop `S.index` 0
      popLen = S.length pop
  indices <- generateIndices (popLen `div` 2) pc
  points <- replicateM popLen $ replicateM points (fromRange indLen)
  return $ multipointCrossoverPure indices points pop

{- Conduit -}
multipointCrossoverConduit ::
  Prob -> -- probability of crossover
  Int -> --number of cross points
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
multipointCrossoverConduit pc points = awaitForever (liftIO . multipointCrossover pc points)

crossoverConduit :: 
  Prob ->
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
crossoverConduit pc = awaitForever (liftIO . crossover pc)

