{-# LANGUAGE  OverloadedStrings #-}
module Conduit where

{-
import qualified Data.Sequence as S
import Data.Conduit as C
import Data.Conduit.TMChan
import Data.Void
import Data.Function
import Data.Traversable as T

import System.Remote.Monitoring
import System.Remote.Counter

import Control.Monad.IO.Class
import Control.Concurrent.STM.TBMChan
import Control.Applicative

import GHC.Conc

import Types
import Crossover
import Rotation
import Evaluation
import PointMutation
import Selection
import UtilsRandom
import Common


-- Draw from 2 conduits and yielding the pair that results
conduit =&&&= conduit' = do
  (a:b:[]) <- sequenceConduits [conduit, conduit']
  C.yield (a, b)
  conduit =&&&= conduit'

nGenerations gens initial conduit = do
  --logger <- liftIO $ getLogger logName
  --handler <- liftIO $ fileHandler logName DEBUG
  --liftIO $ updateGlobalLogger logName (addHandler handler)
  chan <- atomically $ newTBMChan 1
  liftIO . atomically . writeTBMChan chan $ initial
  let loop 0 initial = return initial
      loop gens prev = let gens' = pred gens in
        do val <- await
           case val of
             Nothing -> do
               return prev
             Just a -> do
               if gens' == 0
               then return a
               else do
                 liftIO . atomically . writeTBMChan chan $ initial
                 loop gens' a
    in sourceTBMChan chan $$ loop gens initial

{- Crossover Conduit -}
multipointCrossoverConduit ::
  Prob -> -- probability of crossover
  Int  -> --number of cross points
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
multipointCrossoverConduit pc points = awaitForever (liftIO . rIO . multipointCrossover pc points)

crossoverConduit ::
  Prob -> -- Probability of crossover
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
crossoverConduit pc = awaitForever (liftIO . rIO . crossover pc)


{- Evaluation Conduit -}
evaluationConduit ::
  (Expressed a b -> IO Double) -> -- Evaluation of an individual
  Conduit (Pop (Expressed a b)) IO (Pop (Evaled a b))
evaluationConduit eval = awaitForever $ \ pop -> do
  pop' <- liftIO $ S.zipWith Evaled pop <$> T.mapM eval pop
  C.yield pop'

{- Point Mutation Conduit -}
pointMutationConduit ::
  Prob -> -- Probability of mutation
  Int  -> -- Size of individual
  Int  -> -- Number of bit used in each locus
  Conduit Pop32 IO Pop32
pointMutationConduit pm is bits = awaitForever (liftIO . rIO . pointMutation pm is bits)


{- Rotation Conduit -}
rotationConduit ::
  Prob -> -- Probability of rotation
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
rotationConduit prob = awaitForever (liftIO . rIO . rotation prob)

{- Selection -}
stochasticTournamentConduit ::
  Prob -> -- Probability of selecting the best individual in a tournament
  Conduit (Pop (Evaled a b)) IO (Pop a)
stochasticTournamentConduit prob = awaitForever (liftIO . rIO . stochasticTournament prob)

tournamentSelectionConduit ::
  Int -> -- Tournament size
  Conduit (Pop (Evaled a b)) IO (Pop a)
tournamentSelectionConduit size = awaitForever (liftIO . rIO . tournamentSelection size)

{- Elitism -}
elitismConduit ::
  Int -> -- Number of elite individuals
  (Pop (Evaled a b) -> IO (Pop a)) ->  -- Selection operator
  Conduit (Pop (Evaled a b)) IO (Pop a)
elitismConduit k select = awaitForever $ \ pop -> do
  let (elite, common) = S.splitAt k $ S.sortBy (compare `on` fitness) pop
  selected <- liftIO $ select common
  C.yield (genetics elite S.>< selected)

-}
