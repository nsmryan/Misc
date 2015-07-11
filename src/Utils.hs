{-# LANGUAGE  FlexibleContexts, TypeFamilies #-}
module Utils where

--import Data.Random.Source
--import Data.Random.Source.PureMT
import Data.Random.Sample
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Uniform
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.List.Split

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


{- Diversity Measures -}
--TODO consider incorporating the edit-distance package
--to provide diversity of RGEP phenotypes.
--TODO this maybe should be in another file?
--make this type more generic
--this comes from "Measurements of Population Diversity"

--splits out bits before calculating diversity
wordDiversity :: (Bits n) => Int -> S.Seq (S.Seq n) -> Double
wordDiversity bits iss =
  centroidDiversity $ fmap (S.fromList . F.concat . fmap (expandBits bits)) iss

centroidDiversity :: (Integral n) => S.Seq (S.Seq n) -> Double
centroidDiversity dss = diversity' (S.length dss) $ F.toList $ fmap F.toList $ (fmap (fmap fromIntegral)) dss

diversity' :: Int -> [[Double]] -> Double
diversity' n dss =
  let centroids = map ((/len) . sum) transposed
      transposed = transpose dss
      len = fromIntegral n
  in
    sum $ map (\(ds, c) -> sum $ map (\a -> (a-c)^2) ds) $ zip transposed centroids

{- Efficient application of genetic operators -}
--should use mono-traversable and sequence


skipping n = do
  ((i, b):is) <- get
  if i == 0
    then do
      return 0
    else do
      let (skips, m) = divMod i n
      put $ (m, b) : is
      return $! skips

putBack b i = modify ((i, b):)

takeUnder n = takeUnder' 0 where
  takeUnder' acc = do
    ias <- get
    let i = fst . head $ ias
    let acc' = acc + i
    if null ias || (acc' >= n)
      then return []
      else do
        modify tail
        is <- takeUnder' $ 1 + acc'
        return (acc' : is)

open = downOne . zipper
close = unzipper . upOne

--t s n = trace (s ++ show n) n
--t s n = n

type IxSupply = State [Int]

--applyMOn ::
--  Int ->
--  (a -> IxSupply a) ->
--  S.Seq a ->
--  IxSupply (S.Seq a)
applyMOn :: (Functor m, Monad m, MonadState m, StateType m ~ [(Int, b)]) =>
  Int ->
  (b -> a -> m a) ->
  S.Seq a ->
  m (S.Seq a)
applyMOn n f as =
  close <$> (go (open as)) where
    go as = do
      is <- get
      if null is || (1 + remainingRight as == 0)
        then return as
        else do
          (_, b) <- gets head
          i <- skipping n
          let remaining = remainingRight as
          as' <- modifyWithM (zipRight i as) (f b)
          case compare i (remaining * n) of
            LT -> go $ skipOne as'
            EQ -> return as'
            GT -> do
              putBack b $! (i - remaining - 1)
              return $! as

-- each location, with given location size.
applyOn n f is as = evalState (applyMOn n f' as) is where
  f' b a = do
    modify tail
    return $ f b a

applyOnEach f is as = applyOn 1 f is as

applyFuncs n is as = applyOn n ($) is as
applyFtoIx n fs is as = applyOn n ($) (zip is fs) as

-- 1 layer down, locus size = 1
applyOnLocus n is as = evalState (applyMOn n f' as) is where
  f' _ as' = applyMOn 1 f'' as'
  f'' b a = do
    modify tail
    return $ b a

-- 1 layer down, with given locus size
applyWithinLocus n m f is as = evalState (applyMOn (n*m) f' as) is' where
  is' = map (flip (,) ()) is
  f' _ as' = applyMOn m f'' as'
  f'' _ a = do
    ixs <- takeUnder m
    return $! f ixs a

-- apply function taking index from list
applyIxOn n f is as = evalState (applyMIxOn n f as) is where

-- monadic appyly with function taking index
applyMIxOn n f as = applyMOn n f' as where
  f' a = do
    (i, b) <- gets head
    return (f i b a)

