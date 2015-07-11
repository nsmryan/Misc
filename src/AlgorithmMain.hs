{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module AlgorithmMain where

import Prelude as P

import Data.Configurator as C
import Data.Configurator.Types
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Csv as CSV
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Random

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.STM

import Diagrams.Prelude as D hiding ((<>))
import Diagrams.TwoD.Combinators as DC
--import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG

import Options.Applicative as OPT

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams

import Pipes as P
import Pipes.Safe as PS

import System.FilePath.Posix

import HealMonad
import Common
import UtilsRandom
import Utils
import GA
import RGEP
import PipeAlgorithms
import PointMutation
import Rotation
import Crossover
import Selection
import PipeOperators
import Evaluation
import Types
import Channels as Ch



mainCfgFileName = Optional "main.cfg"
defaultCfgName = Optional "default.cfg"


{- Diagram stuff -}
--bitBlock :: (Renderable (Path R2) b) => Bool -> Diagram b R2
bitBlock b = square 1 # (fc $ if b == 1 then black else white)

--blockGroup :: (Bits b) => Int -> b -> Diagram b R2
bitGroup bitsUsed b = hcat $ map bitBlock $ expandBits bitsUsed b

--line :: (Bits b) => Int -> [b] -> Diagram b R2
line block bs = hcat $ map block bs

--grid :: (Bits b) => Int -> [[b]] -> Diagram b R2
grid block bss = vcat $ map (line block) bss

bitGrid bitsUsed = grid (bitGroup bitsUsed)

grayBlock bitsUsed r = let r' = r * 255 in square 1 # fc (sRGB24 r' r' r')
grayGrid = grid grayBlock

{- Charts stuff -}
lineColors = cycle $ map opaque [blue, red, green, black, orange]
addIndices = zip [0..]
plot (col, (lineTitle, as)) =
  toPlot $ (def :: PlotLines Double Double)
         & plot_lines_title .~ lineTitle
         & plot_lines_style . line_color .~ col
         & plot_lines_values .~ [addIndices as]
layout title plots = (def :: Layout Double Double)
                   & layout_title .~ title
                   & layout_plots .~ plots
mkGraph title as = layout title (map plot (zip lineColors as))

chartCSV denv fileName colNames = do
  fitnessCSV <- BL.readFile fileName
  case CSV.decode HasHeader fitnessCSV of
    Left err -> putStrLn err >> return Nothing
    Right vectData -> do
      let dataSet = F.toList vectData
      let dataPoints = zipWith (,) colNames $ transpose dataSet
      let graph = toRenderable $ mkGraph (dropExtension fileName) dataPoints
      let (fitnessGraph, _) = runBackendR denv graph
      return $ Just fitnessGraph


{- RGEP Main -}
rgepMain :: [Op a] -> (RGEPExpressed a -> Double) -> a -> IO ()
rgepMain ops fitnessFunction defaul = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Get configuration files -}
  config <- loadConfiguration $ configFiles options

  {- Run algorithm -}
  pop <- processRGEP ops fitnessFunction defaul config

  {- Post Process -}
  postProcessRGEP pop
postProcessRGEP pop = do
  denv <- defaultEnv vectorAlignmentFns 1000 1000
  (Just fitnessGraph) <- chartCSV denv "fitness.log" ["Average", "Best"]

  let fitDiagram = textDiagram $ show $ pop

  renderSVG "summary.svg" (Width 1000) $ (fitnessGraph === fitDiagram)
  print pop

textDiagram str = text str # fontSizeL 2 # fc black <> square 100 # fc white

processRGEP :: [Op a] -> (RGEPExpressed a -> Double) -> a -> Config -> IO Pop32
processRGEP ops fitnessFunction defaul config = do
  ps   <- C.lookupDefault (50    :: Int)    config "ps"
  is   <- C.lookupDefault (100   :: Int)    config "is"

  let bits = bitsUsed ops

  initialPopulation <- rIO $ pop32 ps is bits

  mutation       <- runBlock config pmBlock
  crossover1     <- runBlock config crossoverBlock
  crossover2     <- runBlock config (multipointCrossoverBlock 2)
  selection      <- runBlock config tournamentBlock
  rotation       <- runBlock config rotationBlock
  expressionPipe <- runBlock config (rgepExpressionBlock ops defaul)
  fitnessPipe    <- runBlock config (fitnessBlock fitnessFunction)
  generations    <- runBlock config gensBlock
  logFitnessPipe <- runBlock config logFitness

  cycleWith initialPopulation $
    (expressionPipe >-> fitnessPipe >-> logFitnessPipe >-> generations >->
     selection >-> rotation >-> mutation >-> crossover1 >-> crossover2)

  (output, input, seal) <- PS.runSafeT $ rIO $ compileChain $ Link
    (expressionPipe >-> fitnessPipe >-> logFitnessPipe >-> generations >->
     selection >-> rotation >-> mutation >-> crossover1 >-> crossover2)
  result <- Ch.place (output, input) initialPopulation
  atomically seal
  return result


{- Genetic Algorithm Main -}
gaMain :: (Ind32 -> b) -> (b -> Double) -> IO ()
gaMain expressionFunction fitnessFunction = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Pre-processing: Get configuration files -}
  config <- loadConfiguration $ configFiles options

  {- Processing: Run algorithm -}
  pop <- processGA expressionFunction fitnessFunction config

  {- Post-Processing: Generate Data -}
  gaPostProcess pop

gaPostProcess pop = do
  print pop

  let bitmap = bitGrid 1 $ F.toList (fmap F.toList pop)

  diversityCSV <- BL.readFile "diversity.log"
  (Just diversityData) <- case CSV.decode HasHeader diversityCSV of
    Left msg -> putStrLn ("error: " ++ msg) >> return Nothing
    Right diversityData -> return . Just $ diversityData
  let grayMap = grayGrid ((F.toList $ diversityData) :: [[Double]])

  --renderCairo "bits.png" (Width 400) bitmap
  renderSVG "bits.svg" (Width 400) bitmap

  denv <- defaultEnv vectorAlignmentFns 1000 1000
  (Just fitnessGraph) <- chartCSV denv "fitness.log" ["Average", "Best"]
  (Just diversityGraph) <- chartCSV denv "diversity.log" ["Diversity"]
  --renderCairo "summary.png" (Width 1000) $ (fitnessGraph D.||| diversityGraph === bitmap # D.scale 10) # bg black
  renderSVG "summary.svg" (Width 1000) $ (fitnessGraph D.||| diversityGraph === bitmap # D.scale 10) # bg black
  putStrLn "Done"

processGA expressionFunction fitnessFunction config = do
  ps   <- C.lookupDefault (50    :: Int)    config "ps"
  is   <- C.lookupDefault (100   :: Int)    config "is"

  --rIO $ parallelGA 50 1000 100 0.01 0.6 ones
  --runApp config $ geneticAlgorithmApp ones
  --rIO . runStage' 0 . cycleNTimes 1000 $ simpleStage

  initialPopulation <- rIO $ pop32 ps is 1

  mutation       <- runBlock config pmBlock
  crossover      <- runBlock config crossoverBlock
  selection      <- runBlock config tournamentBlock
  expressionPipe <- runBlock config (expressionBlock expressionFunction)
  fitnessPipe    <- runBlock config (simpleFitnessBlock fitnessFunction)
  diversityPipe  <- runBlock config logWordDiversity
  logLocusPipe   <- runBlock config logAvgLocus
  generations    <- runBlock config gensBlock
  logFitnessPipe <- runBlock config logFitness

  cycleWith initialPopulation $
    (diversityPipe >-> logLocusPipe   >-> expressionPipe >->
     fitnessPipe   >-> logFitnessPipe >-> generations    >->
     selection     >-> mutation       >-> crossover)

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
