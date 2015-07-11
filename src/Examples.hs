module Examples where

import Control.Monad.ST
import Control.Monad.Primitive.Class
import Control.Monad
import qualified Control.Concurrent as Con
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Text.Printf

import Data.List
import Data.Ord
import Data.Word
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Bits as B
import Data.Word

import PointMutation
import Utils
import Rotation
import RGEP
import Types
import UtilsRandom
import Evaluation
import GA




{-
--TODO turn these tests into properties/unit test cases
testDecode = do
  let termIndices = map (`B.shiftL` 1) [0..5]
  let nontermIndices = map ((`B.setBit` 0) . (`B.shiftL` 1)) [0..5]
  print termIndices
  print nontermIndices
  putStrLn ""
  print $ map (decode arithOps) termIndices
  print $ map (decode arithOps) nontermIndices

testStacks = do
      let prog = [oneTerm, twoTerm, plusOp, dup, timesOp, twoTerm]
      let progUnderflow = [oneTerm, twoTerm, plusOp, timesOp]
      print $ "safe program"
      print $ map name prog
      print $ "safe program, filtered"
      print $ map name $ filterUnderflows prog
      print $ "safe program, cleaned"
      print $ map name $ cleanProg prog
      print $ "safe program, run unsafe"
      print $ runOpsUnsafe prog
      print $ "safe program, run safe"
      print $ runOps prog
      print $ "unsafe program"
      print $ map name progUnderflow
      print $ "unsafe program, filtered"
      print $ map name $ filterUnderflows progUnderflow
      print $ "unsafe program, cleaned"
      print $ map name $ cleanProg progUnderflow
      print $ "unsafe program, safely"
      print $ runOps progUnderflow
      print $ "unsafe program, unsafely"
      print $ runOpsUnsafe progUnderflow

{-
testRGEPPBIL = do
  let ps = 50
  let is = 20
  let gens = 1000
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  let eval = (0.0 -) . runProgramWithDefault 0.0 . fmap decoder
  (ind, fit, probs) <- rIO $ rgepPBIL ops ps is gens 0.1 0.075 0.02 0.05 eval
  print probs
  putStrLn ""
  print $ fmap name . cleanProg . fmap decoder $ ind
  putStrLn ""
  print $ negate fit
-}

{-
testGA gens = do
  let evaluate ind = return $ ones ind
  population <- rIO $ geneticAlgorithm 10 1000 gens 0.01 0.6 evaluate
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "fitness = %f\n" fit

testRGEP = do
  let ops = [plusOp, timesOp, dup, oneTerm, twoTerm, zeroTerm] :: [Op Double]
      decoder = decode ops
  population <- rIO $ rgep 100 20 ops 0.01 0.1 0.6 0.6 0.75 1000 0 return
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

testRGEPConduit = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
      decoder = decode ops
      gens = 100
      ps = 100
      is = 20
      bits = bitsUsed ops
  population <- rIO $ pop32 ps is bits
  --initial <- rIO $ evaluation (return . rgepRun decoder 0) population
  pop <- nGenerations gens population (rgepConduit is ps ops 0.01 0.1 0.6 0.6 0.75 0 return)
  population' <- rIO $ evaluation (return . rgepRun decoder 0) pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population'
  printf "bits = %s\n" (show bits)
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

-}
-}
