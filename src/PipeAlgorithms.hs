module PipeAlgorithms where

import Math.Probable

import qualified Data.Sequence as S

import Control.Monad

import Pipes

import PipeOperators
import Channels
import UtilsRandom



pipedGA ps is pm pc gens fitness = do
  initialPopulation <- S.replicateM ps $ S.replicateM is $ r $ word32In (0, 1)
  cycleNTimes gens initialPopulation $ geneticAlgorithmP ps is pm pc gens fitness


geneticAlgorithmP ps is pm pc gens fitnessFunction =
  let fitnessPipe = for cat $ \ ind -> yield (ind, fitnessFunction ind)
  in ((stage (for cat each)) >=>
      (stage fitnessPipe) >=>
      (stage (collect ps)) >=>
      (stage (tournamentSelectionP ps 2)) >=>
      (stage (for cat each)) >=>
      (stage (pointMutationP pm 1)) >=>
      (stage (collect ps)) >=>
      (stage (crossoverP pc is)))

parallelGA ps is pm pc gens fitness = do
  initialPopulation <- S.replicateM ps $ S.replicateM is $ r $ word32In (0, 1)
  cycleNTimes gens initialPopulation $ parallelGAP ps is pm pc gens fitness

parallelGAP ps is pm pc gens fitnessFunction =
  let fitnessPipe = for cat $ \ ind -> yield (ind, fitnessFunction ind)
  in ((stage (for cat each)) >=>
      (stations 8 $ stage fitnessPipe) >=>
      (stage (collect ps)) >=>
      (stations 8 $ stage (tournamentSelectionP ps 2)) >=>
      (stage (for cat each)) >=>
      (stations 8 $ stage (pointMutationP pm 1)) >=>
      (stage (collect ps)) >=>
      (stations 8 $ stage (crossoverP pc is)))

geneticAlgorithmDefaultP fitness = geneticAlgorithmP 100 10 0.01 0.8 100 fitness

