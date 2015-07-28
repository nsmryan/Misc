{-# LANGUAGE  FlexibleContexts, TypeFamilies #-}
module Utils where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Control.Applicative
import Control.Monad.State.Lazy

import Common
import Types
import SeqZip


expandBits bitsUsed b = map (fromEnum . testBit b) [0..bitsUsed-1]

failuresWith p u = floor (log u / log (1 - p))

repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

minBy f a b = if f a < f b then a else b
maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = if f a >= f b then a else b

ones :: Ind32 -> Double
ones ind = fromIntegral $ F.sum ind

sumOnes :: Ind32 -> Double
sumOnes ind = ones ind

timesM :: (Monad m) => Int -> a -> (a -> m a) -> m a
timesM i a m = foldl (>=>) return (replicate i m) $ a

puts f = do
  s <- get
  put (f s)

smap :: (a -> b) -> S.Seq a -> S.Seq b
smap = fmap

seqToVect :: S.Seq a -> V.Vector a
seqToVect = V.fromList . F.toList

vectToSeq :: V.Vector a -> S.Seq a
vectToSeq = S.fromList . F.toList



compareFitnesses = compare `on` fitness

fittestIndividual :: (F.Foldable t) => t (Evaled a b) -> (Evaled a b)
fittestIndividual = F.maximumBy compareFitness

--ensure an individual is in a population
ensureElem :: (Eq a) => S.Seq a -> a -> S.Seq a
ensureElem population individual =
  if individual `F.elem` population
    then population
    else individual S.<| (S.take ((S.length population) - 1) population)

ensureElems population individuals =
  F.foldr (flip ensureElem) population individuals

compareFitness :: (Evaled a b) -> (Evaled a b) -> Ordering
compareFitness = compare `on` fitness

sortByFitness = S.sortBy compareFitness

kFittest k pop = S.take k $ sortByFitness pop


{- Diversity Measures -}
--TODO consider incorporating the edit-distance package
--to provide diversity of RGEP phenotypes.
--TODO this maybe should be in another file?
--make this type more generic

--splits out bits before calculating diversity
wordDiversity :: (Bits n) => Int -> S.Seq (S.Seq n) -> Double
wordDiversity bits iss =
  centroidDiversity $ fmap (S.fromList . F.concat . fmap (expandBits bits)) iss

--this comes from "Measurements of Population Diversity"
centroidDiversity :: (Integral n) => S.Seq (S.Seq n) -> Double
centroidDiversity dss = diversity' (S.length dss) $ F.toList $ fmap F.toList $ (fmap (fmap fromIntegral)) dss

diversity' :: Int -> [[Double]] -> Double
diversity' n dss =
  let centroids = map ((/len) . sum) transposed
      transposed = transpose dss
      len = fromIntegral n
  in
    sum $ map (\(ds, c) -> sum $ map (\a -> (a-c)^2) ds) $ zip transposed centroids
