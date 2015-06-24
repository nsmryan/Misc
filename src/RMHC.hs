module RMHC where

import Data.Random
import qualified Data.Sequence as S

import Control.Applicative

import UtilsRandom
import Types
import Utils
import PointMutation

{- Random Mutation Hill Climbing RGEP -}

rmhc ::
  Int -> --Size of individuals
  Int -> --Bits per locus
  Int -> --Number of generations
  Prob -> --Probability of mutation
  (Ind32 -> R Double) -> --Evaluator
  R (Ind32, Double)
rmhc is bits gens mutRate eval = do
  initGenes <- ind32 is bits
  fitness <- eval initGenes
  let loop 0 ind = return ind
      loop n ind@(genes, fitness) = do

        genes' <-
          (flip S.index 0) <$>
          (pointMutation mutRate is bits $ S.singleton genes)

        fitness' <- eval genes'
        --printf $ "fitness = " ++ show fitness'
        let ind' = (genes', fitness')
        loop (n-1) $ maxBy snd ind' ind
    in
      loop gens (initGenes, fitness)

