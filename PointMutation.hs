module PointMutation where

import qualified Data.Sequence as S
import Data.Word
import Data.Bits
import Data.Random
import Data.Random.Lift
import Data.Conduit

import Control.Monad.IO.Class
import Control.Monad

import UtilsRandom
import Types


{- Pure -}
mutateLocus :: [Int] -> Word32 -> Word32
mutateLocus is n = foldr flipBit n is where
  flipBit index n = n `xor` (1 `shiftL` index)

pointMutationPure :: 
  Int -> Int -> [Int] -> Pop32 -> Pop32
pointMutationPure indLength bits indices pop =
  applyOverLocuses indLength bits (repeat mutateLocus) indices pop

{- MonadRandom -}
pointMutationGenerate :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> m [Int]
pointMutationGenerate pm indLength bits =
  generateIndices totalBits pm where totalBits = bits * indLength

pointMutation :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Int -> Pop32 -> m Pop32
pointMutation pm indLength bits pop =
  do indices <- pointMutationGenerate pm indLength bits
     return $ pointMutationPure indLength bits indices pop

{- Conduit -}
pointMutationConduit ::
  Prob -> Int -> Int -> Conduit Pop32 IO Pop32
pointMutationConduit pm is bits = awaitForever (liftIO . pointMutation pm is bits)

{- Configuration -}

