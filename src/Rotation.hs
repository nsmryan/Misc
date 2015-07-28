{-# LANGUAGE OverloadedStrings #-}
module Rotation where

import qualified Data.Sequence as S
import Data.Random
import Data.Tree

import Control.Monad
import Control.Applicative

import Pipes
import Pipes.Prelude as PP

import PipeOperators
import Types
import Utils
import UtilsRandom


{- Rotation -}
rotateIndividual rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  Forest Int ->
  (Pop (Ind a)) ->
  (Pop (Ind a))
rotationPure rotationPoints pop =
  seqLayer (rotateIndividual . rootLabel . (!!0)) rotationPoints pop

rotationGenerate pr is ps =
  generateTree pr $ layer (singleIndex (1/fromIntegral is)) is ps

rotation ::
  (Monad m) =>
  Prob ->
  Int ->
  Int ->
  (Pop (Ind a)) ->
  RVarT m (Pop (Ind a))
rotation pr is ps pop = rotationPure <$> (rotationGenerate pr is ps) <*> return pop

rotationBlock :: Block Pop32 Pop32
rotationBlock = do
  pr <- lookupConfig (0.02 :: Double) "pr"
  is <- lookupConfig (error "individual size was not provided") "is"
  ps <- lookupConfig (error "population size was not provided") "ps"
  return $ PP.mapM (rotation pr is ps)

