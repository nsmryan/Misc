{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  TemplateHaskell #-}
module UtilsRandom where

--import Data.Random.Source
import Data.IORef
import qualified Data.Sequence as S
import Data.Random
import Data.Random.Source

--import Math.Probable

import Pipes.Concurrent
import Pipes.Safe

import System.Random.MWC

import Control.Monad.Primitive
import Control.Applicative
import Control.Monad.State
import Control.Concurrent.Async
import System.Random.MWC (create)

import Common
import Types
import Utils



asyncR  :: RVarT (SafeT IO) a -> RVarT (SafeT IO) (Async a)
asyncR  = liftIO . async . runSafeT . rIO

rIO :: (RandomSource m (Gen RealWorld), MonadIO m) => RVarT m a -> m a
rIO m = do
  mwc <- liftIO $ create
  sampleFrom mwc m

$(randomSource [d|
   instance (RandomSource IO s) => RandomSource (SafeT IO) s where
     getRandomWord8From s = lift $ getRandomWord8From s
     getRandomWord16From s = lift $ getRandomWord16From s
     getRandomWord32From s = lift $ getRandomWord32From s
     getRandomWord64From s = lift $ getRandomWord64From s
     getRandomDoubleFrom s = lift $ getRandomDoubleFrom s
     getRandomNByteIntegerFrom s n = lift $ getRandomNByteIntegerFrom s n
    |])

fromRange n = uniformT 0 (pred n)

--code taken from mwc library
--TODO use log1p and put in main random-fu
{-# INLINE geo0 #-}
geo0 p
  | p == 1          = return 0
  | p >  0 && p < 1 = do q <- stdUniformT
                         return $! floor $ log q / log (1 - p)
  | otherwise       = error "geo0" "probability out of [0,1] range"

{-# INLINE geo1 #-}
geo1 p = do n <- geo0 p
            return $! n + 1


{- Generate Random population -}

ind32 :: (Monad m) => Int -> Int -> RVarT m Ind32
ind32 is numBits = S.replicateM is $ uniformT 0 bits where
  bits = (2 ^ (fromIntegral numBits)) - 1

ind32All0 :: Int -> Ind32
ind32All0 is = S.replicate is 0

pop32 :: (Monad m) => Int -> Int -> Int -> RVarT m Pop32
pop32 ps is bits = S.replicateM ps $ ind32 is bits

pop32All0 :: Int -> Int -> Pop32
pop32All0 ps is = S.replicate ps $ ind32All0 is

indR :: (Monad m) => Int -> RVarT m IndR
indR is = S.replicateM is stdUniformT

popR :: (Monad m) => Int -> Int -> RVarT m PopR
popR ps is = S.replicateM ps $ indR is

indBits :: Int -> RVarT m IndBits
indBits cap = fromRange cap

popBits :: (Monad m) => Int -> Int -> RVarT m PopBits
popBits len cap = S.replicateM len $ indBits cap

generateIndices ::
  (Monad m) =>
  Int -> Prob -> RVarT m [Int]
generateIndices n 0 = return []
generateIndices n p = generateIndices' 0 n p where
  generateIndices' acc n p =
    do i <- geo0 p
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

