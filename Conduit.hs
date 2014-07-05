{-# LANGUAGE  OverloadedStrings #-}
module Conduit where

import qualified Data.Sequence as S
import Data.Conduit as C
import Data.Conduit.TMChan
import Data.Void
import Data.Traversable as T

import System.Remote.Monitoring
import System.Remote.Counter

import Control.Monad.IO.Class
import Control.Concurrent.STM.TBMChan
import Control.Applicative

import GHC.Conc

import Types
import Crossover
import Evaluation
import PointMutation


nGenerations gens server initial conduit = do
  --logger <- liftIO $ getLogger logName
  --handler <- liftIO $ fileHandler logName DEBUG
  --liftIO $ updateGlobalLogger logName (addHandler handler)
  chan <- atomically $ newTBMChan 1
  liftIO . atomically . writeTBMChan chan $ initial
  genCounter <- liftIO $ getCounter "Generations" server 
  let loop 0 initial = return initial
      loop gens prev = let gens' = pred gens in
        do val <- await
           case val of
             Nothing -> do
               return prev
             Just a -> do
               liftIO $ inc genCounter
               if gens' == 0
               then do
                 return a
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


{- Evaluaton Conduit -}
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

