{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module RealMutation where

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

--TODO provide real valued mutation
--guassian as a start
--0-1, -1-1, bounds, unbounded

boundPlus a b = inBounds (a+b)
boundMinus a b = inBounds (a-b)
boundTimes a b = inBounds (a*b)
boundDiv a b = inBounds (a/b)

newtype UnitReal =
  UnitReal { unitReal :: Double } deriving (Num, Eq)

instance Bounded UnitReal where
  maxBound = UnitReal 1.0
  minBound = UnitReal 0.0

instance Num UnitReal where
  (+) = boundPlus
  (-) = boundPlus
  (*) = boundPlus
  abs = 

newtype Switcher =
  Switcher { switcher :: Double } deriving (Num, Eq)

instance Bounded Switcher where
  maxBound = Switcher   1.0
  minBound = Switcher (-1.0)

{- Pure -}

inBounds :: (Bounded a) => a -> a
inBounds a = max minBound (min maxBound a)

-- Pure mutation on a real valued locus
mutateRealLocus ::
  (Bounded a) =>
  (a -> a) -> -- Modification
  a -> -- Locus
  a    -- Mutated value
mutateRealLocus f a = inBounds $ f a

-- Pure implementation of real valued mutation
realMutationPure ::
  (Bounded a) =>
  Int   -> -- The length of the individuals
  [a]   -> -- The list of perterbations for locuses
  [Int] -> -- The list of locus locations to mutate
  Pop a -> -- The population of individuals to mutate
  Pop a    -- A mutated population
pointMutationPure indLength bits indices pop =
  applyFtoIx indLength bits mutateLocus indices pop

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

