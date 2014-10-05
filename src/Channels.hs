module Channels where

import Math.Probable

import Pipes.Concurrent
import Pipes

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM.TMVar

import Data.Monoid

type Stage a b = Input a -> R (Input b)

newtype R a = R { runR :: (RandT IO a) }

instance Monad R where
  return = R . return
  (R ma) >>= f = R $ do
    a <- ma
    runR (f a)

instance MonadIO R where
  liftIO action = R . RandT . const $ action

randomly ma = R ma
asyncR  = liftIO . async . mwc . runR
r = R

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
 
cycleNTimes :: Int -> a -> Stage a a -> R a
cycleNTimes n value stage = do
  (output, input, seal) <- liftIO $ spawn' Single
  input' <- stage input
  outvar <- liftIO $ atomically $ newEmptyTMVar
  cycleNTimes' n input' output value outvar
  result <- liftIO $ atomically $ takeTMVar outvar
  liftIO $ atomically $ seal
  return result

cycleNTimes' n input output value outvar = let
  generationP n = do
    pop <- await
    if n <= 0
      then do
        liftIO $ atomically $ putTMVar outvar pop
        return ()
      else do
        yield pop
        generationP $ pred n
  in do
    asyncR $ do
      runEffect $ (yield value >> hoist liftIO (fromInput input)) >->
                  generationP n >->
                  hoist liftIO (toOutput output)
      liftIO performGC

stage :: Pipe a b R () -> Stage a b
stage pipe input = do
  (output, input') <- liftIO $ spawn Unbounded
  asyncR $ do
    runEffect $ hoist liftIO (fromInput input) >->
                pipe >->
                hoist liftIO (toOutput output)
    liftIO performGC
  return input'

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
  
