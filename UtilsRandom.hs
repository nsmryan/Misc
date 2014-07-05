{-# LANGUAGE  FlexibleContexts #-}
module UtilsRandom where

--import Data.Random.Source
import Data.Random.Source.PureMT
import Data.Random.Sample
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Uniform
import Data.Random
import Data.IORef
import qualified Data.Sequence as S

import Text.Printf

import Control.Applicative
import Control.Monad.State

import System.Random

import Types
import Utils


uniformGeometric :: (MonadRandom m) => Double -> m Int
uniformGeometric p = sample $ failuresWith p <$> stdUniform

geo p = uniformGeometric p

runRandIO m = do
  g <- newPureMT
  ref <- newIORef g
  runRVar m ref

runRandT m g = evalStateT m g

runRand m g = evalState m g

fromRange :: (Enum n, Num n, Distribution Uniform n, MonadRandom m) =>
  n -> m n
fromRange n = sample $ uniform 0 (pred n)

{- Generate Random population -}

stdIndividual :: (Distribution StdUniform a, MonadRandom m) =>
  Int -> m (Ind a)
stdIndividual is = S.replicateM is $ sample stdUniform

stdPopulation :: (Distribution StdUniform a, MonadRandom m) =>
  Int -> Int -> m (Pop (Ind a))
stdPopulation ps is = S.replicateM ps $ stdIndividual is

ind32 :: (MonadRandom m) => Int -> Int -> m Ind32
ind32 is numBits = S.replicateM is $ sample $ uniform 0 bits where
  bits = (2 ^ (fromIntegral numBits)) - 1

pop32 :: (MonadRandom m) => Int -> Int -> Int -> m Pop32
pop32 ps is bits = S.replicateM ps $ ind32 is bits

indR :: (MonadRandom m) => Int -> m IndR
indR is = S.replicateM is $ sample stdUniform

popR :: (MonadRandom m) => Int -> Int -> m PopR
popR ps is = S.replicateM ps $ indR is

indBits :: (MonadRandom m) => Int -> m IndBits
indBits cap = fromRange cap

popBits :: (MonadRandom m) => Int -> Int -> m PopBits
popBits len cap = S.replicateM len $ indBits cap

generateIndices :: (MonadRandom m) =>
  Int -> Prob -> m [Int]
generateIndices n p = generateIndices' 0 n p where
  generateIndices' acc n p =
    do i <- geo p
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
  return $ applyOn n fs indices as

applyRandomly fs p n as = do
    indices <- generateIndices n p
    return $ applyOn n fs indices as

