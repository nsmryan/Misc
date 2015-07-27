{-# LANGUAGE OverloadedStrings #-}
module Rotation where

import qualified Data.Sequence as S
import Data.Random
--import Data.Conduit

import Control.Monad

import Pipes

import PipeOperators
import Types
import Utils
import UtilsRandom


{- Rotation -}
rotateIndividual rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  [Int] ->
  [Int] ->
  (Pop (Ind a)) ->
  (Pop (Ind a))
rotationPure indices rotationPoints pop =
  applyOnEach ($) (zip indices rotations) pop where
    rotations = map rotateIndividual rotationPoints

rotation ::
  (Monad m) =>
  Prob ->
  (Pop (Ind a)) ->
  RVarT m (Pop (Ind a))
rotation pr pop = let
  popLen = S.length pop
  indLen = S.length $ pop `S.index` 0
    in do
      indices <- generateIndices popLen pr
      rotationPoints <- replicateM (length indices) (fromRange indLen)
      return $ rotationPure indices rotationPoints pop

rotationBlock :: Block Pop32 Pop32
rotationBlock = do
  pr <- lookupConfig (0.02 :: Double) "pr"
  is <- lookupConfig (error "individual size was not provided") "is"
  ps <- lookupConfig (error "population size was not provided") "ps"
  return $ rotationP pr is ps

rotationP prob indLength popSize =
  for cat each >->
  withP prob >->
  whenChosen (rotateOp indLength) >->
  collect popSize

rotateOp indLength individual = do
  rotationPoint <- uniformT 0 (indLength-1)
  return $ rotateIndividual rotationPoint individual

