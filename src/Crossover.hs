{-# LANGUAGE OverloadedStrings #-}
module Crossover where

import qualified Data.Sequence as S
import Data.List
import Data.Monoid
import Data.Random
import Data.String
import Data.Maybe
import Data.Tree

import Control.Monad
import Control.Applicative

import qualified Pipes.Prelude as PP
import Pipes

import Types
import Utils
import UtilsRandom
import Common
import PipeOperators

{- Crossover Utilities -}
crossPair crossPoint (a, b) =
  (top S.>< bottom', top' S.>< bottom) where
    (top,  bottom)  = S.splitAt crossPoint a
    (top', bottom') = S.splitAt crossPoint b

unpair = uncurry (S.><) . (fmap fst &&& fmap snd)

foldInHalf s = S.zip top bottom where (top, bottom) = S.splitAt (S.length s `div` 2) s

exchange as bs = exchange' S.empty S.empty as bs

exchange' rest rest' [] [] = (rest, rest')
exchange' rest rest' (a:as) (b:bs) = exchange' (rest S.>< a) (rest' S.>< b) bs as

splits [] as = [as]
splits (p:points) as = top : splits points bottom where
  (top, bottom) = S.splitAt p as

{- One Point Crossover -}
crossover1Pure::
  Forest Int ->
  Pop (Ind a) ->
  Pop (Ind a)
crossover1Pure crossPoints pop =
  unpair $ seqLayer (crossPair . rootLabel . (!!0)) crossPoints $ foldInHalf pop

singlePointCrossoverGenerate pc indLength popLength =
  generateTree pc $ layer (singleIndex (1/fromIntegral indLength))
                          indLength
                          popLength

crossover1 pc indLength popLength pop =
  crossover1Pure <$> singlePointCrossoverGenerate pc indLength popLength <*> return pop

crossoverBlock :: Block Pop32 Pop32
crossoverBlock = do
  pc <- lookForPC
  is <- lookForIS
  ps <- lookForPS
  return $ PP.mapM (crossover1 pc is ps)

{- Multipoint crossover -}

-- Pure multipoint crossover of two individuals
multicrossPair ixs (a, b) =
  let len = S.length a
      points = sort ixs
  in exchange (splits points a) (splits points b)

multiCrossoverPure ::
  Forest Int ->
  Pop (Ind a) ->
  Pop (Ind a)
multiCrossoverPure crossPoints pop =
  unpair $ seqLayer (multicrossPair . map rootLabel) crossPoints $ foldInHalf pop

multiPointCrossoverGenerate numCrossPoints pc indLength popLength =
  generateTree pc $ layer (nIndices numCrossPoints) indLength popLength

multipointCrossover prob numCrossPoints indLength popLength pop =
  multiCrossoverPure <$> multiPointCrossoverGenerate numCrossPoints prob indLength popLength <*> return pop

multipointCrossoverBlock :: Int -> Block Pop32 Pop32
multipointCrossoverBlock points = do
  is <- lookForIS
  ps <- lookForPS
  pc <- lookForPC
  return $ PP.mapM (multipointCrossover pc points is ps)

{- Geometric Crossover -}
geometricCrossoverGenerate pc pg indLength popLength =
  generateTree pc $ layer (indicesLayer pg) indLength popLength

geometricCrossover pc pg indLength popLength pop =
  multiCrossoverPure <$> geometricCrossoverGenerate pg pc indLength popLength <*> return pop

geoCrossoverBlock :: Block Pop32 Pop32
geoCrossoverBlock = do
  is <- lookForIS
  ps <- lookForPS
  pc <- lookForPC
  pg <- lookupConfig (geo0FromN (fromIntegral is/5)) $ "pg"
  return $ PP.mapM (geometricCrossover pc pg is ps)

