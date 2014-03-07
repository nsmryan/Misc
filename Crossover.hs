module Crossover where

import qualified Data.Sequence as S
import Data.Random
import Data.List
import Data.Conduit

import System.Log.Logger

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
  [(Int, Int)] -> Pop (Ind a) -> Pop (Ind a)
crossoverPure crossInfo population = let
  (top, bottom) = S.splitAt midPoint population
  midPoint = S.length population `div` 2
  pairs = S.zip top bottom
  crossedPairs = applyOverIndices crossPair crossInfo pairs
  (firstHalf, secondHalf) = unpair crossedPairs
    in firstHalf S.>< secondHalf

crossover :: (MonadRandom m, Functor m) =>
  Prob -> Pop (Ind a) -> m (Pop (Ind a))
crossover p population = do
  let totalLength = S.length population `div` 2
      len = S.length $ population `S.index` 0
  indices <- generateIndices totalLength p
  crossPoints <- replicateM (length indices) $ fromRange len
  let crossInfo = zip indices crossPoints
  return $ crossoverPure crossInfo population


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
  [(Int, [Int])] -> Pop (Ind a) -> Pop (Ind a)
multipointCrossoverPure crossData pop = let
  pairs = foldInHalf pop
  result = applyOverIndices multicrossPair crossData pairs
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
  let crossData = zip indices points
  return $ multipointCrossoverPure crossData pop

{- Conduit -}
multipointCrossoverConduit ::
  Prob -> -- probability of crossover
  Int -> --number of cross points
  Pop (Ind a) -> --population to crossover
  HealIO (Pop (Ind a))
multipointCrossoverConduit pc points pop = do
  yield $ LogResult DEBUG "Multipoint Crossover started"
  pop' <- liftIO $ multipointCrossover pc points pop
  yield $ LogResult DEBUG "Multipoint Crossover ended"
  return pop'

crossoverConduit :: 
  Prob ->
  Pop (Ind a) ->
  HealIO (Pop (Ind a))
crossoverConduit pc pop = do
  yield $ LogResult DEBUG "Crossover started"
  pop' <- liftIO $ crossover pc pop
  yield $ LogResult DEBUG "Crossover ended"
  return pop'
