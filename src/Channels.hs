module Channels where

import Math.Probable

import Pipes.Concurrent
import Pipes

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
--import Control.Concurrent.STM.TMVar

import Data.Monoid

import UtilsRandom
import Types


type Stage a b = Input a -> R (Input b)

runStage :: a -> Stage a a -> R (Maybe a)
runStage a stage = do
  (output, input, seal) <- liftIO $ spawn' Single
  input' <- stage input
  liftIO $ atomically $ send output a
  result <- liftIO $ atomically $ recv input'
  liftIO $ atomically $ seal
  return result

runStage' :: a -> Stage a a -> R a
runStage' a stage = do
  result <- runStage a stage
  case result of
    Just value -> return value
    Nothing -> error $ "A Stage was run but didn't return a value!"


cycleChannel stage value = do
  (output, input) <- liftIO $ spawn Single
  output' <- stage output
  cycleChannel' input output' value

cycleChannel' input output value = do
  liftIO $ atomically $ send output value
  result <- liftIO $ atomically $ recv input
  case result of
    Just value' -> cycleChannel' input output value'
    Nothing -> return ()
 
cycleNTimes :: Int -> Stage a a -> Stage a a
cycleNTimes n stage downStreamInput = do
  (preQueueOut,  preQueueIn) <- liftIO $ spawn Single
  (postQueueOut, postQueueIn) <- liftIO $ spawn Single
  stageIn <- stage preQueueIn
  asyncR $ cycleNTimes' n downStreamInput stageIn preQueueOut postQueueOut
  return postQueueIn

cycleNTimes' n downStreamIn stageIn preQueueOut postQueueOut = forever $ do
  inValue <- liftIO $ atomically $ recv downStreamIn
  case inValue of
    Just value -> do
      result <- cycleNTimes'' n value stageIn preQueueOut
      liftIO $ atomically $ send postQueueOut result
    Nothing -> error "No data available when running cycleNTimes"

cycleNTimes'' n inValue stageIn preQueueOut = 
  case n of
    0 -> return inValue
    otherwise -> do
      liftIO $ atomically $ send preQueueOut inValue
      outValue <- liftIO $ atomically $ recv stageIn
      case outValue of
        Just value -> cycleNTimes'' (pred n) value stageIn preQueueOut
        Nothing -> error "got a Nothing in cycleNTimes''"


-- Create a stage out of a pipe.
stage :: Pipe a b R () -> Stage a b
stage pipe input = do
  (output, input') <- liftIO $ spawn Unbounded
  asyncR $ do
    runEffect $ hoist liftIO (fromInput input) >->
                pipe >->
                hoist liftIO (toOutput output)
    liftIO performGC
  return input'

-- Simple test stage that yields random numbers
simpleStage = stage $ for cat $ \ i -> do
  randomValue <- lift $ r $ intIn (0, 100)
  yield (i + randomValue)

throughStage arg stage = do
  (output, input) <- spawn Unbounded
  input' <- mwc $ runR $ stage input
  atomically $ send output arg
  atomically $ recv input'

stations n stage input = do
  inputs <- replicateM n (stage input)
  return $ mconcat inputs

station stage input = stations 1 stage input
  
