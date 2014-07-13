{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Function
import Data.Bits
import Data.Word
import Data.Random
import Data.Random.Distribution.Uniform

import Control.Monad
import Control.Applicative

import Text.Printf

import System.Process

import Graphics.GChart

import RGEP
import Types
import UtilsRandom
import Evaluation
import Conduit
import Utils
import GA


maxSizeSimpleEncoding = 61

pm = 0.01
pc1 = 0.6
pc2 = 0.6
pr = 0.01
pt = 0.75

ps = 25
gens = 1000

problemSizes = [10, 100, 1000]
algorithms = ["GA", "RGEP"]

allPairs as bs = concat $ allPairs' as bs
allPairs' [] bs = []
allPairs' as [] = []
allPairs' (a:as) bs = map (\b -> (a,b)) bs : allPairs' as bs

evalGA = return . ones
runGA is = geneticAlgorithm ps is gens pm pc1 evalGA

runRGEP is = let bitsNum = fromIntegral is in
                 rgep ps is (bitSetOps is) pm pc1 pc2 pr pt gens 0 (return . fromIntegral . popCount)

bestFitness = F.maximum . map snd . F.toList
bestInd as = F.maximumBy (compare `on` snd) $ F.toList as

adjustFitness :: Int -> Double -> Int
adjustFitness problemSize fitness = round $ maxSizeSimpleEncoding * (fitness / fromIntegral problemSize)

bitVector :: (MonadRandom m, Applicative m) => Int -> m (V.Vector Bool)
bitVector size = V.fromList <$> (replicateM size $ sample stdUniform)

main = do
  targetBits <- runRandIO $ mapM bitVector problemSizes
  putStrLn "Running GA"
  gaPop <- mapM runRandIO [runGA i | i <- problemSizes]
  let gaResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip gaPop problemSizes]
  print gaPop
  print gaResults

  putStrLn "Running RGEP"
  rgepPop <- mapM runRandIO [runRGEP i | i <- problemSizes]
  let rgepResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip rgepPop problemSizes]
  --print rgepPop
  print rgepResults

  let chart = do setChartType BarVerticalGrouped
                 setChartTitle "Test Chart"
                 setChartSize 500 400
                 setDataEncoding simple
                 addAxis $ makeAxis { axisType = AxisLeft }
                 addAxis $ makeAxis {
                                       axisType = AxisBottom
                                    ,  axisLabels = Just $ map show problemSizes
                                    }
                 addChartData gaResults
                 addColor "ff0066"
                 addChartData rgepResults
                 addColor "ff6600"
                 setLegend $ legend algorithms

  let url = getChartUrl chart
  print . chartData . getChartData $ chart
  runCommand $ "firefox \"" ++ url ++ "\""

  putStrLn "Done"


