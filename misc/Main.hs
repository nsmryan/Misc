{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Main where

import Prelude as P

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Function
import Data.Bits
import Data.Word
import Data.List.Split
import Data.Random as R
import Data.Random.Distribution.Uniform
import Data.HashMap.Strict hiding (map)
import Data.Colour.Palette.ColorSet
import Data.Colour.SRGB as RGB
import Data.Configurator

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.Cairo

import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Concurrent.Async

import Text.Printf

import System.Process
import System.Random.MWC.Monad
import System.Remote.Monitoring
import System.Remote.Counter
import System.Metrics.Distribution as MD
import System.Metrics
import System.Remote.Label as L
import System.Remote.Monitoring.Statsd

import Options.Applicative as OPT

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
prot = 0.01
pt = 0.75

ps = 25
gens = 1000

mainCfgFileName = Optional "main.cfg"


data AppOptions = AppOptions
             {
               chartResults :: Bool
             , printResults :: Bool
             , loggingEnabled :: Bool
             , monitoringEnabled :: Bool
             , configFiles :: [String]
             }

defaultOptions =
  AppOptions
    {
      chartResults = False
    , printResults = False
    , loggingEnabled = False
    , monitoringEnabled = False
    , configFiles = []
    }

parseOptions =
  AppOptions
    <$> switch (short 't'
            <> long "charting"
            <> help "Whether to produce a chart of results" )
    <*> switch (short 'p'
            <> long "printing"
            <> help "Whether to produce a printout of results" )
    <*> switch (short 'l'
            <> long "logging"
            <> help "Whether to produce a log of results" )
    <*> switch (short 'm'
            <> long "monitoring"
            <> help "Whether to turn on monitoring of the application" )
    <*> (splitOn "," <$> (strOption (short 'c'
            <> long "config"
            <> OPT.value []
            <> help "List of configuration files to use" )))


problemSizes = map (1000*) [1..5] --[10, 100, 1000]
algorithms = ["GA", "RGEP"]

rgepOps is = bitSetOps is 

allPairs as bs = concat $ allPairs' as bs
allPairs' [] bs = []
allPairs' as [] = []
allPairs' (a:as) bs = map (\b -> (a,b)) bs : allPairs' as bs

evalGA bits ind = return . fromIntegral . F.sum . fmap fromEnum $ S.zipWith (==) bits ind
runGA bits is = geneticAlgorithm ps is gens pm pc1 (evalGA bits)

rgepEval bits w = return . fromIntegral . popCount $ (w .&. bits)
runRGEP bits is = let bitsNum = fromIntegral is in
                 rgep ps (is*3) (rgepOps is) pm pc1 pc2 prot pt gens 0 (rgepEval bits)

bestFitness = F.maximum . map snd . F.toList
bestInd as = F.maximumBy (compare `on` snd) $ F.toList as

adjustFitness :: Int -> Double -> Int
adjustFitness problemSize fitness = round $ maxSizeSimpleEncoding * (fitness / fromIntegral problemSize)

bitVector :: (MonadRandom m, Applicative m) => Int -> m (V.Vector Bool)
bitVector size = V.fromList <$> (replicateM size $ R.sample stdUniform)

tobits :: V.Vector Bool -> Integer
tobits bs = V.ifoldl' bitter 0 bs where
 bitter acc ix b = (if b then setBit else clearBit) acc ix

toWord32 :: (Enum e) => e -> Word32
toWord32 e = fromIntegral $ fromEnum e

runCommand_ cmd = void $ runCommand cmd

main = do
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
  boolVects <- runRandIO $ mapM bitVector problemSizes
  let wordVects = map (V.map toWord32) boolVects
  let targetBits = map tobits boolVects
  let targetSeqs = map (S.fromList . F.toList) wordVects

  print targetBits

  putStrLn "Running GA"
  L.set activityName  "GA"
  gaPop <- mapM runRandIO [runGA target i | (i, target) <- zip problemSizes targetSeqs]
  let gaResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip gaPop problemSizes]

  inc algsRun

  print $ map bestInd gaPop
  print gaResults
  mapM (MD.add gaDist) $ map bestFitness gaPop

  --Run RGEP
  putStrLn "Running RGEP"
  L.set activityName "RGEP"
  rgepPop <- mapM runRandIO [runRGEP bits i | (i, bits) <- zip problemSizes targetBits]
  let rgepResults = [adjustFitness problemSize (bestFitness pop) | (pop, problemSize) <- zip rgepPop problemSizes]

  print $ map (\ (pop, is) -> first (rgepRun (decode (rgepOps is)) 0) $ bestInd pop) $ zip rgepPop problemSizes
  print rgepResults
  mapM (MD.add rgepDist) $ map bestFitness rgepPop

  inc algsRun

  --Create Chart
  let chart = makeChart [gaResults, rgepResults]

  let url = getChartUrl chart
  print . chartData . getChartData $ chart

  when (chartResults options) $ runCommand_ ("firefox \"" ++ url ++ "\"")

  when (printResults options) $ do
    statistics <- sampleAll $ serverMetricStore server 
    mapM_ print $ zip (keys statistics) (elems statistics)

  renderCairo "bits.png" (Width 400) $ line 1 $ F.toList $ head wordVects
  putStrLn "Done"


{- Diagram stuff -}
bitBlock b =  square 1 # (fc $ if b then black else white)

blockGroup bitsUsed b = hcat $ map (bitBlock . testBit b) [0..bitsUsed-1] 
line bitsUsed bs = hcat $ map (blockGroup bitsUsed) bs

grid :: (Bits b) => Int -> [[b]] -> Diagram B R2
grid bitsUsed bss = vcat $ map (line bitsUsed) bss

{- Chart stuff -}
makeChart ds = do setChartType BarVerticalGrouped
                  setChartTitle "Algorithm Comparison"
                  setChartSize 500 400
                  setDataEncoding simple
                  addAxis $ makeAxis { axisType = AxisLeft }
                  addAxis $ makeAxis {
                                        axisType = AxisBottom
                                     ,  axisLabels = Just $ map show problemSizes
                                     }
                  mapM_ addChartData ds
                  --this repeats after 24 data sets
                  mapM_ (addColor . hexColor . rybColor) [0..length ds - 1]
                  setLegend $ legend algorithms

hexColor :: (RGB.Colour Double) -> String
hexColor color = let (RGB r g b) = toSRGB24 color in printf "%02X%02X%02X" r g b
