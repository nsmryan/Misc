{-# LANGUAGE RankNTypes #-}
module MiscTest where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad.ST
import Control.Monad.Primitive.Class

import System.Random.MWC.Monad as MWC

import Data.List
import Data.Ord
import Data.Word
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import PointMutation
import Utils

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

  , QC.testProperty "point mutation" $
      \ n ->
        F.all (F.all (== 0xFF)) $
          pointMutationPure n 8 [0..(n*8)] $
            S.singleton $
              S.fromList (replicate n 0)

  , QC.testProperty "apply on each index" $
      \ n ->
        F.all (== 0xFF) $
          applyOn 1 (repeat mutateLocus) [0..(n*8)-1] (S.replicate n 0)
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








