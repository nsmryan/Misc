module PointMutation where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random.Lift
import Data.Bits
import Data.Traversable

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad

import UtilsRandom
import Utils
import Types
import Common


{- Pure -}
mutateLocus :: [Int] -> Word32 -> Word32
mutateLocus is n = foldr flipBit n is
flipBit index n = n `xor` (1 `shiftL` index)

pointMutationPure :: Int -> Int -> [Int] -> Pop32 -> Pop32
pointMutationPure indLength bits indices pop =
  applyWithinLocus indLength bits mutateLocus indices pop

{- MonadRandom -}
pointMutationGenerate :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> m [Int]
pointMutationGenerate pm indLength bits =
  generateIndices totalBits pm where totalBits = bits * indLength

pointMutation :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> Pop32 -> m Pop32
pointMutation pm indLength bits pop =
  do indices <- pointMutationGenerate pm indLength bits
     return $ pointMutationPure indLength bits indices pop

pointMutationNaive ::
  (MonadRandom m, Functor m, Applicative m) =>
  Prob -> Int -> Pop32 -> m Pop32
pointMutationNaive pm bits pop =
  (traverse . traverse) (locus bits pm) pop
 
pointMutationNaiveVector ::
  (MonadRandom m, Functor m, Applicative m) =>
  Prob -> Int -> PopV32 -> m PopV32
pointMutationNaiveVector pm bits pop =
  (traverse . traverse) (locus bits pm) pop

locus bits pm l = do
  points <- locus' bits pm 0
  return $ mutateLocus points l

locus' 0 pm b = return []
locus' bits pm b = do
  p <- sample stdUniform
  if p < pm
    then (b:) <$> (locus' (bits-1) pm (b+1))
    else locus' (bits-1) pm (b+1)

{- Configuration -}

