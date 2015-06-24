module GA where

import Types
import UtilsRandom
import Selection
import Crossover
import PointMutation
import Evaluation
import Common



oneBit = 1

{- Genetic Algorithms -}
geneticAlgorithm ::
  Int -> -- Population size
  Int -> -- Individual size
  Int -> -- Generations
  Prob -> -- pm
  Prob -> -- pc
  (Ind32 -> R Double) -> -- Fitness Evaluation
  R (Pop (Evaled Ind32 Ind32))
geneticAlgorithm ps is gens pm pc eval = do
  initialPopulation <- pop32 ps is oneBit
  let loop 0 pop = return pop
      loop gens pop = do
        popEvaled <- evaluation (evalInd eval) (gaExpressed pop)
        popSelected <- tournamentSelection 2 popEvaled
        popCrossed <- crossover pc popSelected
        popMutated <- pointMutation pm is oneBit popCrossed
        loop (pred gens) popMutated
    in
      do finalPopulation <- loop gens initialPopulation
         evaluation (evalInd eval) (gaExpressed finalPopulation)

gaExpress :: a -> Expressed a a
gaExpress ind = Expressed ind ind
gaExpressed pop = fmap gaExpress pop


