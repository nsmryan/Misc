{-# LANGUAGE  FlexibleContexts #-}
module UtilsRandom where

--import Data.Random.Source
import Data.Random.Source.PureMT
import Data.Random.Sample
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Uniform
import Data.Random
import qualified Data.Sequence as S

import Text.Printf

import Control.Applicative
import Control.Monad.State

import System.Random

import Types

failuresWith p u = floor (log u / log (1 - p))

uniformGeometric :: (MonadRandom m) => Double -> m Int
uniformGeometric p = sample $ failuresWith p <$> stdUniform

geo p = uniformGeometric p

runRandTIO m = m

runRandIO :: IO a -> IO a
runRandIO m = m

runRandT m g = evalStateT m g

runRand m g = evalState m g

fromRange :: (Enum n, Num n, Distribution Uniform n, MonadRandom m) =>
  n -> m n
fromRange n = sample $ uniform 0 (pred n)

repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

minBy f a b = if f a < f b then a else b
maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = if f a >= f b then a else b

timesM :: (Monad m) => Int -> a -> (a -> m a) -> m a
timesM i a m = foldl (>=>) return (replicate i m) $ a



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


{- Efficient application of genetic operators -}
--should use mono-traversable and sequence
--variations- 
--generate indices, supply indices
--need extra data, don't need extra data
--supply extra data, generate extra data

skipping :: (Monad m) => Int -> StateT [(Int, a)] m Int
skipping n = do
  ((i, e):as) <- get
  if i == 0
    then return 0
    else do 
      let (skips, m) = divMod i n
      put $ (m,e) : as
      return $! skips

takeUnder n = takeUnder' n 0 where
  takeUnder' n acc =
    do as <- get
       case as of
         [] -> return []
         (i,e):as' -> do
           let acc' = i + acc
           if acc' >= n
             then do
               put $ (acc' - n, e):as'
               return []
             else do
               put as'
               as'' <- takeUnder' n (acc'+1)
               return $ (i, e) : as''
    
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

--TODO clean up and provide a set of useful cases
applyWithData ::
  Int ->
  ([(Int, b)] -> a -> a) ->
  [(Int, b)] ->
  S.Seq a ->
  S.Seq a
applyWithData n f indices as = evalState (go f as) indices where
  go f as = do 
    emptyList <- gets null
    if emptyList
      then return as
      else do
        i <- skipping n
        let (top, bottom) = S.splitAt i as
        if S.null bottom
          then return top
          else do
            is <- takeUnder n
            let a = f is $ S.index bottom 0
            bottom' <- go f $ S.drop 1 bottom
            return $ top S.>< S.singleton a S.>< bottom'

applyOverIndices f = applyWithData 1 f' where
  f' ((_, b):[]) a = f b a 

applyRandomlyOverIndividuals f n dist prob as = do
  indices <- generateIndices n prob
  dataValues <- replicateM (length indices) dist
  let dat = zip indices dataValues
  return $ applyOverIndices f dat as

applyOverLocuses n m f = applyWithData (n*m) (applyWithData m f)

applyOverLocusesNoData n m f ixs as =
  applyWithData (n*m) (applyWithData m (noData f)) ixs' as where
    ixs' = zip ixs (repeat ())

applyRandomlyWithData f dataDist p n = do
  indices <- generateIndices n p
  dataValues <- replicateM (length indices) dataDist
  return $ applyWithData n f $ zip indices dataValues

applyRandomly f p n as = let
  f' = noData f
  in do
    indices <- generateIndices n p
    return $ applyWithData n f' (zip indices (repeat ())) as

noData f dataValues a = f (map fst dataValues) a

