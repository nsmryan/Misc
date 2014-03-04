module PointMutation where

import qualified Data.Sequence as S
import Data.Word
import Data.Bits
import Data.Random

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
pointMutation :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> Pop32 -> m Pop32
pointMutation pm is bits pop =
  let totalBits = bits * is
    in do
      indices <- generateIndices totalBits pm
      return $ pointMutationPure indices bits pop

{- Conduit -}

{- Configuration -}

