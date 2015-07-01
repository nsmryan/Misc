{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module AlgorithmMain where

import Prelude as P

import Data.Configurator as C
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Csv as CSV

import Control.Applicative

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG

import Options.Applicative as OPT

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

import Pipes as P
--import Pipes.Csv
import qualified Pipes.ByteString as PB

import HealMonad
import Common
import UtilsRandom
import Utils
import GA
import RGEP
import PipeAlgorithms
import PipeOperators
import Evaluation
import Types
import Channels



mainCfgFileName = Optional "main.cfg"
defaultCfgName = Optional "default.cfg"


{- Diagram stuff -}
--bitBlock :: (Renderable (Path R2) b) => Bool -> Diagram b R2
bitBlock b =  square 1 # (fc $ if b then black else white)

--blockGroup :: (Bits b) => Int -> b -> Diagram b R2
blockGroup bitsUsed b = hcat $ map (bitBlock . testBit b) [0..bitsUsed-1] 

--line :: (Bits b) => Int -> [b] -> Diagram b R2
line bitsUsed bs = hcat $ map (blockGroup bitsUsed) bs

--grid :: (Bits b) => Int -> [[b]] -> Diagram b R2
grid bitsUsed bss = vcat $ map (line bitsUsed) bss

{- Charts stuff -}
addIndices = zip [0..]
plot = toPlot $ (def :: PlotLines Double Double)
           & plot_lines_title .~ "test line"
           & plot_lines_values .~ [(addIndices [0..10])]
layout = (def :: Layout Double Double)
             & layout_title .~ "Test Chart"
             & layout_plots .~ [plot]



{- RGEP Main -}
rgepMain :: ((a -> Double) -> R Double) -> IO ()
rgepMain fitnessFunction = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Get configuration files -}
  config <- loadConfiguration $ configFiles options

  pm   <- C.lookupDefault 0.001 config "pm"
  pr   <- C.lookupDefault 0.6   config "pr"
  pc1  <- C.lookupDefault 0.6   config "pc1"
  pc2  <- C.lookupDefault 0.6   config "pc2"
  pt   <- C.lookupDefault 0.75  config "pt"
  gens <- C.lookupDefault 1000  config "gens"
  ps   <- C.lookupDefault 50    config "ps"
  is   <- C.lookupDefault 200   config "is"

  {- Run algorithm -}
  let ops = polyOps
  pop <- rIO $ rgep ps is ops pm pr pc1 pc2 pt gens (const 0) fitnessFunction
  print . genetic . expressed . bestInd $ pop
  print $ "gens " ++ show gens

{- Genetic Algorithm Main -}
gaMain :: (Ind32 -> Double) -> IO ()
gaMain fitnessFunction = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Pre-processing: Get configuration files -}
  config <- loadConfiguration $ configFiles options
  
  {- Processing: Run algorithm -}
  pop <- processGA fitnessFunction config

  {- Post-Processing: Generate Data -}
  print pop 

  let bitmap = grid 1 $ F.toList (fmap F.toList pop)
  renderCairo "bits.png" (Width 400) bitmap
  renderSVG "bits.svg" (Width 400) bitmap

  --fitnessCSV <- BL.readFile "fitness.log"
  --case CSV.decode HasHeader fitnessCSV of
  --  Left err -> putStrLn err
  --  Right fitnessData -> do
  --    renderableToFile def "test.svg" $ toRenderable layout


processGA fitnessFunction config = do
  pm   <- C.lookupDefault (0.001 :: Double) config "pm"
  pc   <- C.lookupDefault (0.6   :: Double) config "pc"
  gens <- C.lookupDefault (100   :: Int)    config "gens"
  ps   <- C.lookupDefault (50    :: Int)    config "ps"
  is   <- C.lookupDefault (100   :: Int)    config "is"

  --rIO $ parallelGA 50 1000 100 0.01 0.6 ones
  --runApp config $ geneticAlgorithmApp ones
  --rIO . runStage' 0 . cycleNTimes 1000 $ simpleStage 

  --initialPopulation <- rIO $ S.replicateM ps $ S.replicateM is $ r $ word32In (0, 1)
  initialPopulation <- rIO $ pop32 ps is 1

  mutation    <- runBlock config pmBlock
  crossover   <- runBlock config crossoverBlock
  selection   <- runBlock config tournamentBlock
  fitnessPipe <- runBlock config (fitnessBlock fitnessFunction)
  generations <- runBlock config gensBlock

  cycleWith initialPopulation $
    (fitnessPipe >-> logFitness >-> generations >-> selection >-> mutation >-> crossover)

  --rIO $ pipedGA ps is gens pm pc fitnessFunction

    

{- Configuration Files -}

loadConfiguration configs =
  let configs' = mainCfgFileName : defaultCfgName : map Required (P.filter (/= "") configs)
  in load configs'

bestFitness = F.maximum . map fitness . F.toList

bestInd :: (F.Foldable f) => f (Evaled a b) -> Evaled a b
bestInd as = F.maximumBy compareFitnesses $ F.toList as

{- Parsing -}

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
