module PointMutation where

import qualified Data.Sequence as S
import Data.Word
import Data.Bits
import Data.Random
import Data.Random.Lift
import Data.Conduit

import System.Log.Logger

import Control.Monad.IO.Class
import Control.Monad

import UtilsRandom
import Types


{- Pure -}
mutateLocus :: [Int] -> Word32 -> Word32
mutateLocus is n = foldr flipBit n is where
  flipBit index n = n `xor` (1 `shiftL` index)

pointMutationPure :: 
  [Int] -> Int -> Pop32 -> Pop32
pointMutationPure indices bits pop = let
  inds = S.length pop
  in applyOverLocusesNoData inds bits mutateLocus indices pop

{- MonadRandom -}
pointMutationGenerate :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> m [Int]
pointMutationGenerate pm is bits =
  generateIndices totalBits pm where totalBits = bits * is

pointMutation :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> Pop32 -> m Pop32
pointMutation pm is bits pop =
  do indices <- pointMutationGenerate pm is bits
     return $ pointMutationPure indices bits pop

{- Conduit -}
pointMutationConduit ::
  Prob -> Int -> Int -> Pop32 -> ConduitM () (Result Pop32) IO Pop32
pointMutationConduit pm is bits pop = do
  yield $ LogResult DEBUG "Point Mutation started"
  pop' <- liftIO $ pointMutation pm is bits pop
  yield $ LogResult DEBUG "Point Mutation ended"
  return pop'


{- Configuration -}

