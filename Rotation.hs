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
rotIndividual rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  [Int] ->
  [Int] ->
  (Pop (Ind a)) ->
  (Pop (Ind a))
rotationPure indices rotationPoints pop =
  applyOver (map rotIndividual rotationPoints) indices pop

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

{- Conduit -}
rotationConduit ::
  Prob -> 
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
rotationConduit prob = awaitForever (liftIO . rotation prob)

