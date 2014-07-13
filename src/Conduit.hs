{-# LANGUAGE  OverloadedStrings #-}
module Conduit where

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
  Int -> --number of cross points
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
multipointCrossoverConduit pc points = awaitForever (liftIO . multipointCrossover pc points)

crossoverConduit :: 
  Prob ->
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
crossoverConduit pc = awaitForever (liftIO . crossover pc)


{- Evaluation Conduit -}
evaluationConduit :: 
  (a -> IO Double) ->
  Conduit (Pop a) IO (Pop (a, Double))
evaluationConduit eval = awaitForever $ \ pop -> do
  pop' <- liftIO $ S.zip pop <$> T.mapM eval pop
  C.yield pop'

{- Point Mutation Conduit -}
pointMutationConduit ::
  Prob -> Int -> Int -> Conduit Pop32 IO Pop32
pointMutationConduit pm is bits = awaitForever (liftIO . pointMutation pm is bits)


{- Rotation Conduit -}
rotationConduit ::
  Prob -> 
  Conduit (Pop (Ind a)) IO (Pop (Ind a))
rotationConduit prob = awaitForever (liftIO . rotation prob)

{- Selection -}
stochasticTournamentConduit ::
  Prob ->
  Conduit (Pop (a, Double)) IO (Pop a)
stochasticTournamentConduit prob = awaitForever (liftIO . stochasticTournament prob)

tournamentSelectionConduit :: 
  Int ->
  Conduit (Pop (Ind a, Double)) IO (Pop (Ind a))
tournamentSelectionConduit size = awaitForever (liftIO . tournamentSelection size)

{- Elitism -}
elitismConduit ::
  Int ->
  (Pop (a, Double) -> IO (Pop a)) -> 
  Conduit (Pop (a, Double)) IO (Pop a)
elitismConduit k select = awaitForever $ \ pop -> do
  let (elite, common) = S.splitAt k $ S.sortBy (compare `on` snd) pop
  selected <- liftIO $ select common
  C.yield (fmap fst elite S.>< selected)
    
