{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Function
import Data.Bits
import Data.Word
import Data.Conduit

import Text.Printf

import System.Remote.Monitoring

import Network

import Control.Monad
import qualified Control.Concurrent as Con
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import RGEP
import Types
import UtilsRandom
import Evaluation
import Conduit


testDecode = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
  let termIndices = map (`shiftL` 1) [0..5]
  let nontermIndices = map ((`setBit` 0) . (`shiftL` 1)) [0..5]
  print termIndices
  print nontermIndices
  putStrLn ""
  print $ map (decode ops) termIndices
  print $ map (decode ops) nontermIndices

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

testRGEPPBIL = do
  let ps = 50
  let is = 20
  let gens = 1000
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  let eval = (0.0 -) . runProgramWithDefault 0.0 . fmap decoder
  (ind, fit, probs) <- runRandIO $ rgepPBIL ops ps is gens 0.1 0.075 0.02 0.05 eval
  print probs
  putStrLn ""
  print $ fmap name . cleanProg . fmap decoder $ ind
  putStrLn ""
  print $ negate fit

ones :: S.Seq Word32 -> Double
ones ind = fromIntegral $ F.sum ind

testGA = do
  let evaluate ind = return $ ones ind
  pop <- runRandIO $ geneticAlgorithm 10 20 1000 0.01 0.6 evaluate
  population <- runRandIO $ evaluation evaluate pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "fitness = %f\n" fit

testRGEP = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
      decoder = decode ops
  pop <- runRandIO $ rgep 50 100 ops 0.01 0.1 0.6 0.6 0.75 1000 0 return
  population <- runRandIO $ evaluation (return . rgepRun decoder 0) pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

testRGEPConduit server = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
      decoder = decode ops
      gens = 0
      ps = 50
      is = 100
      bits = bitsUsed ops
  population <- liftIO $ pop32 ps is bits
  --initial <- runRandIO $ evaluation (return . rgepRun decoder 0) population
  pop <- nGenerations gens server population (rgepConduit is ps ops 0.01 0.1 0.6 0.6 0.75 0 return)
  population' <- runRandIO $ evaluation (return . rgepRun decoder 0) pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population'
  printf "bits = %s\n" (show bits)
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

main = do
  server <- forkServer "localhost" 8000
  let serverID = serverThreadId server
  testRGEPConduit server
  Con.killThread serverID
