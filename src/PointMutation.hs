{-# LANGUAGE OverloadedStrings #-}
module PointMutation where

import qualified Data.Sequence as S
import Data.Bits
import qualified Data.Traversable as T
import Data.Random
import Data.Bits.Lens
import Data.Tree

import Pipes as P
import Pipes.Prelude as PP
import Pipes.Safe

import Control.Applicative
import Control.Monad
import Control.Lens.Operators

import UtilsRandom
import Utils
import Types
import Common
import PipeOperators


-- Pure mutation on a word-length bitmap
{-# INLINE mutateLocus #-}
mutateLocus ::
  (Bits b) =>
  [Int]  -> -- List of bit locations to flip
  b -> -- Bitmap to mutate
  b    -- Mutated bitmap
mutateLocus is n = foldr (flip complementBit) n is

--TODO any function? through lens? monadically?
mutateWith f = mutateWith' f
mutateWith' f _ a = f a

mutateGenericWith lens f = mutateGenericWith' lens f
mutateGenericWith' lens f _ a = a (lens .~ f)

-- Pure implementation of the point mutation genetic operator
pointMutationBitsPure ::
  (Bits b) =>
  Forest Int ->
  Pop (Ind b) ->
  Pop (Ind b)
pointMutationBitsPure forest pop =
  seqLayer (seqLayer (mutateLocus . concatMap flatten)) forest pop

--Generate tree of locations
pointMutationGenerate prob bitsUsed indLength popLength =
  generateTree prob $ layer (layer leafLayer bitsUsed) indLength popLength

--Point Mutation on bits
pointMutationBits prob bitsUsed indLength popLength pop = do
  pointMutationBitsPure <$> pointMutationGenerate prob bitsUsed indLength popLength <*> return pop

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

{- Pipes and Blocks -}
data PMChunk f a = PMPiece (f a)
                 | PMLocation a [Int]
                 | PMFlush deriving (Show, Eq)

pmBlock :: (Bits b) => Block (Pop (Ind b)) (Pop (Ind b))
pmBlock = do
  pm <- lookupConfig 0.01 "pm"
  ps <- lookupConfig (error "population size not provided") "ps"
  is <- lookupConfig (error "individual size not provided") "is"
  geneSize <- lookupConfig (error "gene size not provided") "bitsUsed"
  return $ PP.mapM (pointMutationBits pm geneSize is ps)


--pmPopulationP :: Int -> Double -> Int -> Pipe Pop32 Pop32 (RVarT (SafeT IO)) ()
--pmPopulationP ps pm geneSize = onElementsP ps (pointMutationP pm geneSize)
--
----pointMutationP :: Double -> Int -> Pipe Ind32 Ind32 (RVarT m) r
--pointMutationP prob geneSize =
--  probablyDeep prob geneSize >-> mutateLocusP mutateLocus >-> reconstitute
--
--
--probablyDeep prob locationSize = do
--  loc <- P.lift $ geo0 prob
--  piece <- await
--  probablyDeep' prob locationSize loc piece
--
--probablyDeep' prob size loc ind = do
--  let len = S.length ind * size
--  if len > loc
--    then do
--      let (gene, locus) = loc `divMod` size
--          (top, bottom) = S.splitAt gene ind
--      yield . PMPiece $ top
--      loc' <- processPiece prob size locus (S.index bottom 0) []
--      probablyDeep' prob size loc' $ S.drop 1 bottom
--    else do
--      yield . PMPiece $ ind
--      yield PMFlush
--      ind' <- await
--      probablyDeep' prob size (loc - len) ind'
--
--processPiece prob geneSize locusLoc gene ls = do
--  loc' <- P.lift $ geo0 prob
--  let left = geneSize - locusLoc
--  if loc' < left
--    then do
--      let withinGene = loc' + locusLoc
--          ls' = (withinGene:ls)
--      processPiece prob geneSize withinGene gene ls'
--    else do
--      yield $ PMLocation gene (locusLoc:ls)
--      return $ loc'-left
--
--{- Redone mutation. Should replace other operators with these -}
----for each >-> singleOut p >-> mutateLocusP mutateLocus >-> reconstitute
--
--singleOut p =
--  forever $ singleOutWith p Nothing Nothing
--
--singleOutWith p maybeIndex maybeAs = do
--  index <- case maybeIndex of
--             Nothing -> lift $ geo0 p
--             Just i -> return i
--  as <- case maybeAs of
--          Nothing -> await
--          Just someAs -> return someAs
--  if index >= (S.length as)
--     then do yield (PMPiece as) >> yield PMFlush
--             singleOutWith p (Just (index - S.length as)) Nothing
--     else let (front, back) = S.splitAt index as
--              locus = S.index back 0
--              remaining = S.drop 1 back
--           in do yield $ PMPiece front
--                 yield $ PMLocation locus [index]
--                 singleOutWith p Nothing (Just remaining)
--
--mutateLocusPM f = forever $ do
--  chunk <- await
--  case chunk of
--    PMLocation a indices -> do
--      a' <- lift $ f indices a
--      yield $ Location a'
--    PMPiece piece -> yield . Piece $ piece
--    PMFlush -> yield Flush
--
--mutateLocusPM' f = mutateLocusP (const f)
--
--mutateLocusP f = mutateLocusPM (\is a -> return $ f is a)
--
--mutateLocusP' f = mutateLocusP (\is a -> f a)


