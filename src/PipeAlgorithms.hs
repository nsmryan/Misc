{-# LANGUAGE OverloadedStrings #-}
module PipeAlgorithms where

import Math.Probable

import qualified Data.Sequence as S

import Control.Monad

import Pipes

import PipeOperators
import Channels
import UtilsRandom
import RGEP
import Types
import Evaluation



pipedGA ps is gens pm pc fitness = do
  initialPopulation <- S.replicateM ps $ S.replicateM is $ r $ word32In (0, 1)
  runStage' initialPopulation $ cycleNTimes gens $ geneticAlgorithmP ps is gens pm pc fitness


geneticAlgorithmP ps is gens pm pc fitnessFunction =
  let fitnessPipe = for cat $ \ ind -> yield (ind, fitnessFunction ind)
  in (stage (for cat each >->
             fitnessPipe >->
             collect ps >->
             tournamentSelectionP ps 2 >->
             pmPopulationP ps pm 1 >->
             crossoverP pc is))


parallelGA ps is gens pm pc fitness = do
  initialPopulation <- S.replicateM ps $ S.replicateM is $ r $ word32In (0, 1)
  runStage' initialPopulation $ cycleNTimes gens $ parallelGAP ps is gens pm pc fitness

--TODO this terminates without producing a result
--may have to do with a stage ending early?
--add stages back to get parallelism
parallelGAP ::
  Int    -> -- Population size
  Int    -> -- Individual size
  Int    -> -- Number of generations to run
  Double -> -- Probabilty of point mutation
  Double -> -- Probabilty of crossover
  (Ind32 -> Double) -> -- Evaluation
  Stage Pop32 Pop32
parallelGAP ps is gens pm pc fitnessFunction =
  let fitnessPipe = undefined --for cat $ \ ind -> yield (ind, fitnessFunction ind)
  in (--(stage (for cat each)) >=>
      --(stage fitnessPipe) >=>
      --(stage (collect ps)) >=>
      --((fitnessLogger True True)) >=>
      --(stage (tournamentSelectionP ps 2)) >=>
      (stage (pmPopulationP ps pm 1)) >=>
      (stage (crossoverP pc is)))

geneticAlgorithmDefaultP fitness = geneticAlgorithmP 100 10 100 0.01 0.8 fitness

--TODO fill out loggers as a start for logging capability
--fitnessLogger logAvg logBest = 
--  let avgPipe = if logAvg then avgLogger else cat 
--      bestPipe = if logBest then bestLogger else cat 
--  in
--    avgPipe >-> bestPipe

--avgLogger = for cat undefined -- $ \ population -> $ do
  --undefined

--bestLogger = for cat undefined -- $ \ population -> $ do
  --undefined

{-
-- The types are slightly off here. "a" is forced to Ind32 for some reason.
-- This seems to have to do with the type of one of the operators
rgepP ::
  Int -> --population size
  Int -> --individual size
  [Op a] -> --operators
  Prob -> -- pm
  Prob -> -- pr
  Prob -> -- pc1
  Prob -> -- pc2
  Prob -> -- tournament selection prob
  Int -> -- generations
  a -> -- default value
  (a -> R Double) ->
  R Pop32
-}
rgepP ps is ops pm pr pc1 pc2 pt gens def eval = do
  let bits = bitsUsed ops
      decoder = decode ops
      rgepEvaluation pop = undefined -- evaluation (evalInd eval) (fmap (rgepRun decoder def) pop)
  pop <- pop32 ps is bits
  runStage' pop $ cycleNTimes gens $ parallelRGEP ps is bits gens pm pr pc1 pc2 eval

parallelRGEP ps is bits gens pm pr pc1 pc2 fitnessFunction =
  let fitnessPipe = for cat $ \ ind -> do fitness <- lift $ fitnessFunction ind
                                          yield (ind, fitness)
  in ((stage (for cat each)) >=>
      (stations 8 $ stage fitnessPipe) >=>
      (stage (collect ps)) >=>
      (stage (stochasticTournamentSelectionP 0.8 ps 2)) >=>
      (stage (for cat each)) >=>
      (stations 8 $ stage (pointMutationP pm bits)) >=>
      (stations 8 $ stage (rotationP pm is)) >=>
      (stage (collect ps)) >=>
      (stations 8 $ stage (crossoverP pc1 is)))

