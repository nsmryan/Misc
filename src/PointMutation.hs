{-# LANGUAGE OverloadedStrings #-}
module PointMutation where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random.Lift
import Data.Bits
import qualified Data.Traversable as T
import Data.Random

import Pipes as P
import Pipes.Safe

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad

import UtilsRandom
import Utils
import Types
import Common
import PipeOperators


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
  (Monad m) =>
  Prob -> -- The probability of point mutation
  Int  -> -- The length of each individual
  Int  -> -- The number of bits used in each locus
  RVarT m [Int] -- A random like of locations to mutate within the individual
pointMutationGenerate pm indLength bits =
  generateIndices totalBits pm where totalBits = bits * indLength

-- Point Mutation operator, efficient implementation
pointMutation ::
  (Monad m) =>
  Prob  -> -- Probability of mutation
  Int   -> -- Length of the individual
  Int   -> -- Number of bits used in each locus
  Pop32 -> -- Population to muate
  RVarT m Pop32  -- Mutated population
pointMutation pm indLength bits pop =
  do indices <- pointMutationGenerate pm indLength bits
     return $ pointMutationPure indLength bits indices pop

-- Point Mutation operator, naive implementation for testing
pointMutationNaive ::
  (Monad m) =>
  Prob  -> -- Probability of mutation
  Int   -> -- Number of bits used in each locus
  Pop32 -> -- Population to mutate
  RVarT m Pop32  -- Mutated population
pointMutationNaive pm bits pop =
  (T.traverse . T.traverse) (locus bits pm) pop

-- Point Mutation operator, naive implementation with vectors.
pointMutationNaiveVector ::
  Prob -> Int -> PopV32 -> RVarT m PopV32
pointMutationNaiveVector pm bits pop =
  (T.traverse . T.traverse) (locus bits pm) pop

-- Mutate a single locus
locus bits pm l = do
  points <- locus' bits pm 0
  return $ mutateLocus points l

locus' 0 pm b = return []
locus' bits pm b = do
  p <- stdUniformT
  if p < pm
    then (b:) <$> (locus' (bits-1) pm (b+1))
    else locus' (bits-1) pm (b+1)

--locusP :: (Monad m, Foldable f) => Pipe (f a) a m r
locusP prob size pipe =
  for cat each >-> locusP' prob size pipe >-> collect size

locusP' prob size pipe =
  probablyDeep prob size >-> pipe >-> reconstitute


{- Pipes andd Blocks -}
data PMChunk f a = PMPiece (f a)
                 | PMLocation a [Int]
                 | PMFlush deriving (Show, Eq)

pmBlock :: Block Pop32 Pop32
pmBlock = do
  pm <- lookupConfig 0.001 "pm"
  ps <- lookupConfig (error "population size not provided") "ps"
  geneSize <- lookupConfig (error "gene size not provided") "bitsUsed"
  return $ pmPopulationP ps pm geneSize


pmPopulationP :: Int -> Double -> Int -> Pipe Pop32 Pop32 (RVarT (SafeT IO)) r
pmPopulationP ps pm geneSize = --locusP pm geneSize (pointMutationP pm geneSize)
  (for cat each) >-> (pointMutationP pm geneSize) >-> (collect ps)

--pointMutationP :: Double -> Int -> Pipe Ind32 Ind32 (RVarT m) r
pointMutationP prob geneSize = --locusP prob geneSize pointMutationP'
  probablyDeep prob geneSize >-> pointMutationP' >-> reconstitute


probablyDeep prob locationSize = do
  loc <- P.lift $ geo0 prob
  piece <- await
  probablyDeep' prob locationSize loc piece

probablyDeep' prob size loc ind = do
  let len = S.length ind * size
  if len > loc
    then do
      let (gene, locus) = loc `divMod` size
          (top, bottom) = S.splitAt gene ind
      yield . PMPiece $ top
      loc' <- processPiece prob size locus (S.index bottom 0) []
      probablyDeep' prob size loc' $ S.drop 1 bottom
    else do
      yield . PMPiece $ ind
      yield PMFlush
      ind' <- await
      probablyDeep' prob size (loc - len) ind'

processPiece prob geneSize locusLoc gene ls = do
  loc' <- P.lift $ geo0 prob
  let left = geneSize - locusLoc
  if loc' < left
    then do
      let withinGene = loc' + locusLoc
          ls' = (withinGene:ls)
      processPiece prob geneSize withinGene gene ls'
    else do
      yield $ PMLocation gene (locusLoc:ls)
      return $ loc'-left

pointMutationP' = forever $ do
  chunk <- await
  case chunk of
    PMLocation a is -> do
      yield . Location . mutateLocus is $ a
    PMPiece piece -> yield . Piece $ piece
    PMFlush -> yield Flush
