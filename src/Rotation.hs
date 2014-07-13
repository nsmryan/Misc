module Rotation where

import qualified Data.Sequence as S
import Data.Random
import Data.Conduit

import Control.Monad.IO.Class

import Control.Monad

import Types
import Utils
import UtilsRandom


{- Rotation -}
rotate rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  [Int] ->
  [Int] ->
  (Pop (Ind a)) ->
  (Pop (Ind a))
rotationPure indices rotationPoints pop =
  applyOnEach ($) (zip indices rotations) pop where
    rotations = map rotate rotationPoints

rotation :: (Functor m, MonadRandom m) =>
  Prob -> 
  (Pop (Ind a)) ->
  m (Pop (Ind a))
rotation pr pop = let
  popLen = S.length pop
  indLen = S.length $ pop `S.index` 0
    in do
      indices <- generateIndices popLen pr
      rotationPoints <- replicateM (length indices) (fromRange indLen)
      return $ rotationPure indices rotationPoints pop

