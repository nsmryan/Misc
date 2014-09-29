module Crossover where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random
import Data.List
import Data.Monoid
import Data.Traversable

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Control.Parallel

import Types
import Utils
import UtilsRandom

{- Crossover -}
crossPair crossPoint (a, b) = 
  (top S.>< bottom', top' S.>< bottom) where
    (top,  bottom)  = S.splitAt crossPoint a
    (top', bottom') = S.splitAt crossPoint b
      
unpair as = (fmap fst as, fmap snd as)

crossApplyOn :: 
  [Int] -> [Int] -> Pop (Ind a) -> Pop (Ind a)
crossApplyOn indices crossPoints population = let
  pairs = foldInHalf population 
  crossedPairs = applyOnEach ($) (zip indices (map crossPair crossPoints)) pairs
  (firstHalf, secondHalf) = unpair crossedPairs
    in firstHalf S.>< secondHalf

crossover :: (MonadRandom m, Functor m) =>
  Prob -> Pop (Ind a) -> m (Pop (Ind a))
crossover p population = do
  let totalLength = S.length population `div` 2
      len = S.length $ population `S.index` 0
  indices <- generateIndices totalLength p
  crossPoints <- replicateM (length indices) $ fromRange len
  return $ crossApplyOn indices crossPoints population

crossoverVectWith ::
  (Prob -> ((IndV a, IndV a) -> RVar (IndV a, IndV a))) ->
  Prob ->
  PopV (IndV a) ->
  RVar (PopV (IndV a))
crossoverVectWith crosser p population = do
  let len = V.length population
  let (front, back) = V.splitAt (len `div` 2) population
  let pairs = V.zip front back 
  crossed <- traverse (crosser p) pairs
  let unpaired = (V.map fst crossed) `mappend` (V.map snd crossed)
  return unpaired

crossNaiveVect pc inds@(ind, ind') = do
  p <- sample stdUniform
  if p < pc
    then do
      point <- sample (uniform 0 (V.length ind)) 
      let (front, back) = V.splitAt point ind
      let (front', back') = V.splitAt point ind'
      return (front `mappend` front', back' `mappend` back)
    else return inds

crossNaiveVectPar pc inds@(ind, ind') = do
  p <- sample stdUniform
  if p < pc
    then do
      point <- sample (uniform 0 (V.length ind)) 
      let (front, back) = V.splitAt point ind
      let (front', back') = V.splitAt point ind'
      let crossed  = front `mappend` front'
      let crossed' = back' `mappend` back
      return $ crossed `par` crossed' `pseq` (crossed, crossed')
    else return inds

crossVectSeq = crossoverVectWith crossNaiveVect 
crossVectPar = crossoverVectWith crossNaiveVectPar

crossoverSeqWith ::
  (Prob -> (Ind a, Ind a) -> RVar (Ind a, Ind a)) ->
  Prob ->
  Pop (Ind a) ->
  RVar (Pop (Ind a))
crossoverSeqWith crosser p population = do
  let len = S.length population
  let (front, back) = S.splitAt (len `div` 2) population
  let pairs = S.zip front back 
  crossed <- traverse (crosser p) pairs
  let unpaired = (fmap fst crossed) `mappend` (fmap snd crossed)
  return unpaired

crossoverSeq = crossoverSeqWith crossSeq
crossoverPar = crossoverSeqWith crossPar

crossSeq pc inds@(ind, ind') = do
  p <- sample stdUniform
  if p < pc
    then do
      point <- sample (uniform 0 (S.length ind)) 
      let (front, back) = S.splitAt point ind
      let (front', back') = S.splitAt point ind'
      let crossInd = front `mappend` front' 
      let crossInd' = back' `mappend` back
      return $ (crossInd, crossInd')
    else return inds

crossPar pc inds@(ind, ind') = do
  p <- sample stdUniform
  if p < pc
    then do
      point <- sample (uniform 0 (S.length ind)) 
      let (front, back) = S.splitAt point ind
      let (front', back') = S.splitAt point ind'
      let crossInd = front `mappend` front' 
      let crossInd' = back' `mappend` back
      return $ crossInd `par` crossInd' `pseq` (crossInd, crossInd')
    else return inds

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
  result = applyOnEach ($) (zip crossIndices (map multicrossPair crossPoints)) pairs
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

