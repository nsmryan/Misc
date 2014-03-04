module RMHC where

import Data.Random

import UtilsRandom
import Types
import PointMutation

{- Random Mutation Hill Climbing RGEP -}

rmhc :: (MonadRandom m, Functor m) =>
  Int -> --Size of individuals
  Int -> --Bits per locus
  Int -> --Number of generations
  Prob -> --Probability of mutation
  (Ind32 -> m Double) -> --Evaluator
  m (Ind32, Double)
rmhc is bits gens mutRate eval = do
  initGenes <- ind32 is $ fromIntegral bits
  fitness <- eval initGenes
  let loop 0 ind = return ind
      loop n ind@(genes, fitness) = do
        genes' <- applyRandomly mutateLocus mutRate (bits*is) genes
        fitness' <- eval genes'
        let ind' = (genes', fitness)
        loop (pred n) (maxBy snd ind' ind)
    in
      loop gens (initGenes, fitness)

