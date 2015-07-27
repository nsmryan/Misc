{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Main where

import Prelude as P

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Bits
import Data.Word
import Data.HashMap.Strict hiding (map)
import Data.Colour.SRGB as RGB
import Data.Configurator as C
import Data.Random

import Diagrams.Prelude hiding ((<>))
--import Diagrams.Backend.Cairo

import Control.Monad

import Text.Printf

import Math.Polynomial

import System.Process
import System.Remote.Monitoring
import System.Remote.Counter
import System.Metrics.Distribution as MD
import System.Metrics
import System.Remote.Label as L

import Options.Applicative as OPT


import RGEP
import Types
import UtilsRandom
import GA
import AlgorithmMain

--main function should be able to choose some default problems and
--algorithms

trainingSet = [(i, i^2 + 3*i + 1) | i <- [0..10.0]]
main = mainWith (processRGEP ops defaul fitnessFunction) postProcessRGEP where
  --ops = bitSetOps 32
  ops = let bits = 32 in unionOp bits:setBitTerms bits
  defaul = 0
  fitnessFunction = (fromIntegral . popCount . rgepTreeless . expression)
  --fitnessFunction = ((safeDiv 1) . errorOn trainingSet . evalPoly . rgepTreeless . expression)

--main = gaMain id sumOnes

maxSizeSimpleEncoding = 61

pm = 0.01
pc1 = 0.6
pc2 = 0.6
prot = 0.01
pt = 0.75

ps = 25
gens = 100



problemSizes = map (100*) [1..2] --[10, 100, 1000]
algorithms = ["GA", "RGEP"]

rgepOps is = bitSetOps is

allPairs as bs = concat $ allPairs' as bs
allPairs' [] bs = []
allPairs' as [] = []
allPairs' (a:as) bs = map (\b -> (a,b)) bs : allPairs' as bs

evalGA bits ind =
  let fitness = fromIntegral . F.sum . fmap fromEnum $ S.zipWith (==) bits ind
  in return $ fitness

runGA bits is = geneticAlgorithm ps is gens pm pc1 id (evalGA bits)

rgepEval bits w = fromIntegral . popCount $ (w .&. bits)
runRGEP bits is = let bitsNum = fromIntegral is
  in rgep ps (is*3) (rgepOps is) pm pc1 pc2 prot pt gens 0 (rgepEval bits)

adjustFitness :: Int -> Double -> Int
adjustFitness problemSize fitness = round $ maxSizeSimpleEncoding * (fitness / fromIntegral problemSize)

bitVector :: Int -> RVarT m (V.Vector Bool)
bitVector size = V.fromList <$> (replicateM size stdUniformT)

tobits :: V.Vector Bool -> Integer
tobits bs = V.ifoldl' bitter 0 bs where
 bitter acc ix b = (if b then setBit else clearBit) acc ix

toWord32 :: (Enum e) => e -> Word32
toWord32 e = fromIntegral $ fromEnum e

vect2Seq vect = S.fromList . F.toList $ vect

runCommand_ cmd = void $ runCommand cmd

miscMain = do
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  let configs = mainCfgFileName : map Required (P.filter (/= "") (configFiles options))
  config <- load configs

  --Start monitoring
  --TODO turn this into a Maybe Server where it is Nothing if monitoring is disabled.
  server <- forkServer "localhost" 8000

  --register monitored values
  algsRun <- getCounter "Algorithms Run" server
  activityName <- getLabel "Current Acvitity" server
  gaDist <- getDistribution "GA Distribution" server
  rgepDist <- getDistribution "RGEP Distribution" server

  L.set activityName  "Setting Up"

  --Run GA
  boolVects <- rIO $ mapM bitVector problemSizes
  let wordVects = map (V.map toWord32) boolVects
  let targetBits = map tobits boolVects
  let targetSeqs = map vect2Seq wordVects

  print targetBits

  putStrLn "Running GA"
  L.set activityName  "GA"
  gaPop <- mapM rIO [runGA target i | (i, target) <- zip problemSizes targetSeqs]
  let gaResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip gaPop problemSizes]

  inc algsRun

  print $ map bestInd gaPop
  print gaResults
  mapM (MD.add gaDist) $ map bestFitness gaPop

  --Run RGEP
  putStrLn "Running RGEP"
  L.set activityName "RGEP"
  --rgepPop <- mapM rIO [runRGEP bits i | (i, bits) <- zip problemSizes targetBits]
  --let rgepResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip rgepPop problemSizes]

  --print $ map (\ (pop, is) -> first (rgepRun (decode (rgepOps is)) 0) $ bestInd pop) $ zip rgepPop problemSizes
  --print rgepResults
  --mapM (MD.add rgepDist) $ map bestFitness rgepPop

  inc algsRun

  --Create Chart
  --let chart = makeChart [gaResults, rgepResults]

  --let url = getChartUrl chart
  --print . chartData . getChartData $ chart

  --when (chartResults options) $ runCommand_ ("firefox \"" ++ url ++ "\"")

  when (printResults options) $ do
    statistics <- sampleAll $ serverMetricStore server
    mapM_ print $ zip (keys statistics) (elems statistics)

  --renderCairo "bits.png" (Width 400) $ line 1 $ F.toList $ head wordVects
  putStrLn "Done"



hexColor :: (RGB.Colour Double) -> String
hexColor color = let (RGB r g b) = toSRGB24 color in printf "%02X%02X%02X" r g b

