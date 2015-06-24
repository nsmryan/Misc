{-# LANGUAGE OverloadedStrings #-}
module HealMonad where

import Data.Configurator as C
import Data.Configurator.Types

import Control.Monad.Reader.Class
import Control.Monad.Reader

import Channels
import PipeAlgorithms
import UtilsRandom
import Types


--The final operator should do its own call to "stations" so that,
--for example crossover won't have termination issues because too
--many individuals went to one stage

type App a = ReaderT Config IO a
type Operator a b = App (Stage a b)

data HealConfig = HealConfig { configFIle :: Config }

{-
-- Add monitoring server handle, log file server
data Context = Config { configFIle :: Config }
type Operator a b = ReaderT Context IO (Stage a b)
-}


runOperator config initializer operator = do
  stage <- runReaderT config operator
  runStage initializer stage

runApp config ma = runReaderT ma config

{-

consider having several built-in fitness functions for each algorithm like
data GAProblems = BinaryClassifier | Ones | RoyalRoad
and a way of specifying specific algorithms
data Algorithms = RGEP | GA | UGA | PSO

requiring a "dataSet" file in the config, 
and then having a version that is calleable from the command line to perform these.

GA Concept:
fullGA fitnessFunction = do
  ps <- lookForDefault "ps" 25
  ps <- lookForDefault "is" 100
  ps <- lookForDefault "generations" 100
  ps <- lookForDefault "pm" 0.01
  ps <- lookForDefault "pc" 0.6
  parallelGAP ps is gens pm pc fitnessFunction
-}

lookForDefault :: (Configured a) => Name -> a -> App a
lookForDefault name def = do
  config <- ask
  liftIO $ lookupDefault def config name

lookFor name def = do
  config <- ask
  liftIO $ C.lookup config name
  
geneticAlgorithmApp ::
  (Ind32 -> Double) -> -- Fitness function
  ReaderT Config IO Pop32
geneticAlgorithmApp fitnessFunction = do
  ps <- lookForDefault "ps" 25
  is <- lookForDefault "is" 100
  gens <- lookForDefault "generations" 100
  pm <- lookForDefault "pm" 0.01
  pc <- lookForDefault "pc" 0.6
  liftIO $ rIO $ parallelGA ps is gens pm pc fitnessFunction
  
