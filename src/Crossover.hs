{-# LANGUAGE OverloadedStrings #-}
module Crossover where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.List
import Data.Monoid
import qualified Data.Traversable as T
import Data.Random
import qualified Data.Text as T
import Data.String

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Control.Parallel

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe

import Types
import Utils
import UtilsRandom
import Common
import PipeOperators

{- Crossover -}
--pure crossover of two individuals
crossPair crossPoint (a, b) =
  (top S.>< bottom', top' S.>< bottom) where
    (top,  bottom)  = S.splitAt crossPoint a
    (top', bottom') = S.splitAt crossPoint b

unpair = uncurry (S.><) . (fmap fst &&& fmap snd)

--pure single point crossover of populations
singlePointCrossover ::
  [Int] -> [Int] -> Pop (Ind a) -> Pop (Ind a)
singlePointCrossover indices crossPoints population = let
  pairs = foldInHalf population
  crossedPairs = applyOnEach ($) (zip indices (map crossPair crossPoints)) pairs
  in unpair crossedPairs

--Singlepoint Crossover of a population
singlePointCrossoverM ::
  (Monad m) =>
  Prob -> Pop (Ind a) -> RVarT m (Pop (Ind a))
singlePointCrossoverM p population = do
  let totalLength = S.length population `div` 2
      len = S.length $ population `S.index` 0
  indices <- generateIndices totalLength p
  crossPoints <- replicateM (length indices) $ fromRange len
  return $ singlePointCrossover indices crossPoints population

crossoverBlock :: Block Pop32 Pop32
crossoverBlock = do
  pc <- lookForPC
  return . P.mapM . singlePointCrossoverM $ pc
  --return $ crossoverP pc ps is

--crossoverP :: Double -> Int -> Int -> Pipe Pop32 Pop32 (RVarT (SafeT IO)) r
--crossoverP prob ps is =
--  P.map foldInHalf >-> probably prob >-> crossoverP' is >-> reconstitute >-> collect ps >-> P.map unpair
--
--crossoverP' indLength = forever $ do
--  chunk <- await
--  case chunk of
--    Location pair -> do
--      crossPosition <- lift $ uniformT 0 (indLength-1)
--      yield . Location . crossPair crossPosition $ pair
--    a -> yield a
--
--crossoverIndividualsP prob indLength =
--  onPairsP (withP prob >-> whenChosen (crossoverIndividualP indLength))
--
--crossoverIndividualP indLength pair = do
--  point <- uniformT 0 (indLength-1)
--  return $ crossPair point pair

{- Multipoint crossover -}
foldInHalf s = S.zip top bottom where (top, bottom) = S.splitAt (S.length s `div` 2) s

exchange as bs = exchange' S.empty S.empty as bs

exchange' rest rest' [] [] = (rest, rest')
exchange' rest rest' (a:as) (b:bs) = exchange' (rest S.>< a) (rest' S.>< b) bs as

splits [] as = [as]
splits (p:points) as = top : splits points bottom where
  (top, bottom) = S.splitAt p as

-- Pure multipoint crossover of two individuals
multicrossPair ixs (a, b) =
  let len = S.length a
      points = sort ixs
  in exchange (splits points a) (splits points b)

-- Pure multipoint crossover
multipointCrossover ::
  [Int] -> [[Int]] -> Pop (Ind a) -> Pop (Ind a)
multipointCrossover crossIndices crossPoints pop = let
  pairs = foldInHalf pop
  result = applyOnEach ($) (zip crossIndices (map multicrossPair crossPoints)) pairs
  in unpair result

-- Multipoint crossover
multipointCrossoverM ::
  (Monad m) =>
  Prob        -> --probability of crossover
  Int         -> --number of cross points
  Pop (Ind a) -> --population to crossover
  RVarT m (Pop (Ind a))
multipointCrossoverM pc points pop = do
  let indLen = S.length $ pop `S.index` 0
      popLen = S.length pop
  indices <- generateIndices (popLen `div` 2) pc
  points <- replicateM popLen $ replicateM points (fromRange indLen)
  return $ multipointCrossover indices points pop

-- MultiPoint Crossover
multipointCrossoverBlock :: Int -> Block Pop32 Pop32
multipointCrossoverBlock points = do
  pc <- lookupConfig 0.06 $ "pc" <> (fromString $ show points)
  return . P.mapM $ multipointCrossoverM pc points
  --return $ crossoverMultipointP pc is points

--crossoverMultipointP prob indLength points =
--  onElementsP (onPairsP (probably prob >-> crossoverMultiP' indLength points >-> reconstitute))
--
--
--crossoverMultiP' indLength points = do
--  chunk <- await
--  case chunk of
--    Location pair -> do
--      crossPositions <- lift $ replicateM points $ uniformT 0 (indLength-1)
--      yield . Location . multicrossPair crossPositions $ pair
--    a -> yield a
