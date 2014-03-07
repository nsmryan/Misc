module Rotation where

import qualified Data.Sequence as S
import Data.Random
import Data.Conduit

import System.Log.Logger

import Control.Monad.IO.Class

import Control.Monad

import Types
import UtilsRandom


{- Rotation -}
rotIndividual rotationPoint ind = let
  (top, bottom) = S.splitAt rotationPoint ind
   in bottom S.>< top

rotationPure ::
  [(Int, Int)] ->
  (Pop (Ind a)) ->
  (Pop (Ind a))
rotationPure rotPoints pop =
  applyOverIndices rotIndividual rotPoints pop

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
      let rotationData = zip indices rotationPoints
      return $ rotationPure rotationData pop

{- Conduit -}
rotationConduit ::
  Prob -> 
  Pop (Ind a) ->
  HealIO (Pop (Ind a))
rotationConduit prob pop = do
  yield $ LogResult DEBUG "Rotation started"
  pop' <- liftIO $ rotation prob pop
  yield $ LogResult DEBUG "Rotation ended"
  return pop'

