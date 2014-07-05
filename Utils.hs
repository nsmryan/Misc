{-# LANGUAGE  FlexibleContexts #-}
module Utils where

--import Data.Random.Source
import Data.Random.Source.PureMT
import Data.Random.Sample
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Uniform
import qualified Data.Sequence as S

import Text.Printf

import Control.Applicative
import Control.Monad.State

import Types
import SeqZip


--TODO think aboue using RWS Int [Int] (Seq a)
--reading size, taking from indices, and writting used up sequence
--perhaps a supply of functions to apply- constant or from list


failuresWith p u = floor (log u / log (1 - p))

repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

minBy f a b = if f a < f b then a else b
maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = if f a >= f b then a else b

timesM :: (Monad m) => Int -> a -> (a -> m a) -> m a
timesM i a m = foldl (>=>) return (replicate i m) $ a


{- Efficient application of genetic operators -}
--should use mono-traversable and sequence


skipOver n = do
  is <- get
  case is of
    [] -> return 0
    (i:is) -> return $ i `div` n

skipping :: Int -> State [Int] Int
skipping n = do
  (i:is) <- get
  if i == 0
    then do
      return 0
    else do 
      let (skips, m) = divMod i n
      --if (m /= 0)
      --  then put $ m : is
      --  else put is
      put $ m : is
      return $! skips

putBack i = modify (i:)

open = downOne . zipper
close = unzipper . upOne

--t s n = trace (s ++ show n) n
t s n = n

applyMOn ::
  Int ->
  (a -> State [Int] a) ->
  S.Seq a ->
  State [Int] (S.Seq a)
applyMOn n f as =
  close <$> (go (open (t "as = " as))) where
    go as = do
      is <- get
      if null (t ("n = " ++ show n ++ ", is: ") is) || (1 + remainingRight as == 0)
        then return as
        else do
          i <- skipping n
          let remaining = remainingRight as
          as' <- modifyWithM (zipRight (t "applying right by: " i) as) f
          case compare i remaining of
            LT -> go $ skipOne as'
            EQ -> return as'
            GT -> do
              putBack $! t "putting back " (i - remaining - 1)
              return $! as

-- each location, with given location size.
applyOn n f is as = evalState (applyMOn n f' as) is where
  f' a = do
    modify tail
    return $ f a

-- each location, with given location size.
applyOnEach = applyOn

-- 1 layer down, locus size = 1
applyOnLocus n f is as = evalState (applyMOn n f' as) is where
  f' as' = applyMOn 1 f'' as'
  f'' a = do
    modify tail
    return $ f a

-- 1 layer down, with given locus size
applyWithinLocus n m f is as = evalState (applyMOn (n*m) f' as) is where
  f' as' = applyMOn m f'' as'
  f'' as'' = do
    i <- gets head
    puts tail
    return (f i)

-- apply function taking index from list
applyIxOn n f is as = evalState (applyMIxOn n f as) is where

-- monadic appyly with function taking index
applyMIxOn n f as = applyMOn n f' as where
  f' a = do
    i <- gets head
    puts tail
    return (f i a)

puts f = do
  s <- get
  put (f s)


