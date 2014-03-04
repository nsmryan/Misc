module Rotation where

import qualified Data.Sequence as S
import Data.Random

import Control.Monad

import Types
import UtilsRandom


{- Rotation -}
rotIndividual rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  [(Int, Int)] ->
  Pop32 ->
  Pop32
rotationPure rotPoints pop =
  applyOverIndices rotIndividual rotPoints pop

rotation :: (Functor m, MonadRandom m) =>
  Prob -> 
  Pop32 ->
  m Pop32
rotation pr pop = let
  popLen = S.length pop
  indLen = S.length $ pop `S.index` 0
    in do
      indices <- generateIndices popLen pr
      rotationPoints <- replicateM (length indices) (fromRange indLen)
      let rotationData = zip indices rotationPoints
      return $ rotationPure rotationData pop

