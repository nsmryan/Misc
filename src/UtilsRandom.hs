{-# LANGUAGE  FlexibleContexts #-}
module UtilsRandom where

--import Data.Random.Source
import Data.IORef
import qualified Data.Sequence as S

import Math.Probable

import Pipes.Concurrent

import Control.Applicative
import Control.Monad.State
import Control.Concurrent.Async

import Common
import Types
import Utils



randomly ma = R ma

asyncR  :: R a -> R (Async a)
asyncR  = liftIO . async . mwc . runR

r = R
rIO = mwc . runR

fromRange n = r $ intIn (0, (pred n))

{- Generate Random population -}

ind32 :: Int -> Int -> R Ind32
ind32 is numBits = r $ S.replicateM is $ word32In (0, bits) where
  bits = (2 ^ (fromIntegral numBits)) - 1

ind32All0 :: Int -> Ind32
ind32All0 is = S.replicate is 0

pop32 :: Int -> Int -> Int -> R Pop32
pop32 ps is bits = S.replicateM ps $ ind32 is bits

pop32All0 :: Int -> Int -> Pop32
pop32All0 ps is = S.replicate ps $ ind32All0 is 

indR :: Int -> R IndR
indR is = R $ S.replicateM is double

popR :: Int -> Int -> R PopR
popR ps is = S.replicateM ps $ indR is

indBits :: Int -> R IndBits
indBits cap = fromRange cap

popBits :: Int -> Int -> R PopBits
popBits len cap = S.replicateM len $ indBits cap

generateIndices ::
  Int -> Prob -> R [Int]
generateIndices n 0 = return []
generateIndices n p = generateIndices' 0 n p where
  generateIndices' acc n p =
    do i <- r $ geometric0 p
       let acc' = i + acc
       case acc' >= n of
         True -> return []
         False -> do
           is <- generateIndices' (acc'+1) n p 
           return $ i:is

applyRandomlyOverIndividuals f n dist prob as = do
  indices <- generateIndices n prob
  dataValues <- replicateM (length indices) dist
  let fs = fmap f dataValues
  return $ applyFtoIx n fs indices as

applyRandomly fs p n as = do
    indices <- generateIndices n p
    return $ applyFtoIx n fs indices as

