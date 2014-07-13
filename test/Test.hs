{-# LANGUAGE RankNTypes #-}
module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad.ST
import Control.Applicative
import Control.Monad.Primitive.Class

import System.Random.MWC.Monad as MWC

import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Random.Source.PureMT

import Common
import PointMutation
import Utils
import UtilsRandom
import Rotation as R
import RMHC
import GA


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
    QC.testProperty "randomness test" $
      \v n ->
        (n > 0) QC.==>
          between 0 n
          (runST $ runRTest v (MWC.uniformR (0, n :: Int)))

  , QC.testProperty "mutate all points" $
      \ n ->
        F.all (F.all (== 0xFF)) $
          pointMutationPure n 8 (replicate (n*8) 0) $ pop32All0 1 n
              
  , QC.testProperty "mutate every other" $
      \ n m -> (n > 0 && m > 0) QC.==>
        F.all (F.all (== 0xAA)) $
          pointMutationPure n 8 (replicate (n*m*8) 1) $ pop32All0 m n

  , QC.testProperty "Rotation Wraps" $
      \ as -> 
        let seq = S.fromList (as :: [Int]) in seq == R.rotate (S.length seq) seq

  , QC.testProperty "Zero Rotation is id" $
      \ as -> 
        let seq = S.fromList (as :: [Int]) in seq == R.rotate 0 seq

  , QC.testProperty "Indices Stay in Range" $
      \ n g -> n > 0 QC.==>
        n >= (sum . map (1+) $ (runRandPure (generateIndices n 0.5) (pureMT g)))

  , QC.testProperty "Indices at 0.5" $
      \ n g -> n > 0 QC.==>
        let n' = n * 10000 in 
          (n' `div` 2) `closeTo` (length $ (runRandPure (generateIndices n' 0.5) (pureMT g)))

  , QC.testProperty "Indices at 1.0" $
      \ n g -> n > 0 QC.==>
          n == (length $ (runRandPure (generateIndices n 1.0) (pureMT g)))

  , QC.testProperty "Indices at 0.0" $
      \ n g -> n > 0 QC.==>
          0 == (length $ (runRandPure (generateIndices n 0) (pureMT g)))

  , QC.testProperty "Mutation is Random" $
      \ g g' -> g /= g' QC.==>
          let n = 100 in 
            (runRandPure (generateIndices n 0) (pureMT g))
            /=
            (runRandPure (generateIndices n 0) (pureMT g'))

  {-
  , QC.testProperty "GA > RMHC" $
      \ g -> let rmResult = runRandPure (rmhc 100 1 100 0.01 (return . ones)) (pureMT g)
                 gaResult = runRandPure (geneticAlgorithm 20 100 100 0.01 0.6 (return . ones)) (pureMT g)
             in
               (F.maximum $ (ones <$> gaResult))
               >
               snd rmResult
  -}
  ]

unitTests = testGroup "Unit tests"
  [
    testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , testCase "Mutate All" $ 0xFF @?= (mutateLocus [0..7] 0)
  , testCase "Mutate Some" $ 0xAA @?= (mutateLocus [1, 3, 5, 7] 0)
  ]

between a b c = (b >= c) && (a <= c)

runRTest v m = runWithVector (asRandST m) (V.fromList v)

closeTo n m = between 0.9 1.1 $ (fromIntegral m) / (fromIntegral n)






