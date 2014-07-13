module GA where

import Data.Random

import Types
import UtilsRandom
import Selection
import Crossover
import PointMutation
import Evaluation

{- Genetic Algorithms -}
geneticAlgorithm :: (Functor m, MonadRandom m) =>
  Int -> -- Population size
  Int -> -- Individual size
  Int -> -- Generations
  Prob -> -- pm
  Prob -> -- pc
  (Ind32 -> m Double) -> -- Fitness Evaluation
  m (Pop (Ind32, Double))
geneticAlgorithm ps is gens pm pc eval = do
  initialPopulation <- pop32 ps is 1
  let loop 0 pop = return pop
      loop gens pop = do
        popEvaled <- evaluation eval pop
        popSelected <- tournamentSelection 2 popEvaled
        popCrossed <- crossover pc popSelected
        popMutated <- pointMutation pm is 1 popCrossed
        loop (pred gens) popMutated
    in
      do finalPopulation <- loop gens initialPopulation
         evaluation eval finalPopulation


