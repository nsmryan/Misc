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

-- Pure mutation on a word-length bitmap
mutateLocus ::
  [Int]  -> -- List of bit locations to flip
  Word32 -> -- Bitmap to mutate
  Word32    -- Mutated bitmap
mutateLocus is n = foldr flipBit n is
flipBit index n = n `xor` (1 `shiftL` index)

-- Pure implementation of the point mutation genetic operator
pointMutationPure ::
  Int   -> -- The length of the individuals
  Int   -> -- The number of bits used in each locus
  [Int] -> -- The list of bit locations to mutate within the individual
  Pop32 -> -- The population of individuals to mutate
  Pop32    -- A mutated population
pointMutationPure indLength bits indices pop =
  applyWithinLocus indLength bits mutateLocus indices pop

{- Random -}

-- Randomly generate locations to mutate within an individual
pointMutationGenerate ::
  Prob -> -- The probability of point mutation
  Int  -> -- The length of each individual
  Int  -> -- The number of bits used in each locus
  R [Int] -- A random like of locations to mutate within the individual
pointMutationGenerate pm indLength bits =
  generateIndices totalBits pm where totalBits = bits * indLength

-- Point Mutation operator, efficient implementation
pointMutation ::
  Prob  -> -- Probability of mutation
  Int   -> -- Length of the individual
  Int   -> -- Number of bits used in each locus
  Pop32 -> -- Population to muate
  R Pop32  -- Mutated population
pointMutation pm indLength bits pop =
  do indices <- pointMutationGenerate pm indLength bits
     return $ pointMutationPure indLength bits indices pop

-- Point Mutation operator, naive implementation for testing
pointMutationNaive ::
  Prob  -> -- Probability of mutation
  Int   -> -- Number of bits used in each locus
  Pop32 -> -- Population to mutate
  R Pop32  -- Mutated population
pointMutationNaive pm bits pop =
  (traverse . traverse) (locus bits pm) pop
 
-- Point Mutation operator, naive implementation with vectors.
pointMutationNaiveVector ::
  Prob -> Int -> PopV32 -> R PopV32
pointMutationNaiveVector pm bits pop =
  (traverse . traverse) (locus bits pm) pop

-- Mutate a single locus
locus bits pm l = do
  points <- locus' bits pm 0
  return $ mutateLocus points l

locus' 0 pm b = return []
locus' bits pm b = do
  p <- r $ double
  if p < pm
    then (b:) <$> (locus' (bits-1) pm (b+1))
    else locus' (bits-1) pm (b+1)

{- Configuration -}

