module GA where

import Data.Random

import Types
import UtilsRandom
import Selection
import Crossover
import PointMutation
import Evaluation


oneBit = 1

{- Genetic Algorithms -}
geneticAlgorithm ::
  (Monad m) =>
  Int -> -- Population size
  Int -> -- Individual size
  Int -> -- Generations
  Prob -> -- pm
  Prob -> -- pc
  (Ind32 -> a) -> -- Expression to type "a"
  (a -> RVarT m Double) -> -- Fitness Evaluation
  RVarT m (Pop (Evaled Ind32  a))
geneticAlgorithm ps is gens pm pc expr eval = do
  initialPopulation <- pop32 ps is oneBit
  let loop 0 pop = return pop
      loop gens pop = do
        popExpressed <- return $ expressPopWith expr pop
        popEvaled <- evaluationM eval popExpressed
        popSelected <- tournamentSelection 2 popEvaled
        popCrossed <- singlePointCrossoverM is ps pc popSelected
        popMutated <- pointMutation pm is oneBit popCrossed
        loop (pred gens) popMutated
    in
      do finalPopulation <- loop gens initialPopulation
         evaluationM eval (expressPopWith expr finalPopulation)



