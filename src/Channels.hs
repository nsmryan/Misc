{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Channels where

import Pipes.Concurrent
import Pipes

import Control.Concurrent.Async as Async
import Control.Monad.Loops as CL
import Control.Applicative

import Data.Maybe
import Data.Random
import Data.Random.Distribution.Uniform

import Types


type Stage m a b = Input a -> RVarT m (Input b)

data Chain m a b where
  Link        :: (Pipe a b m ()) -> Chain m a b
  Chain       :: (Chain m a b) -> (Chain m b c) -> Chain m a c
  Cycle       :: (Chain m a (Either a a)) -> Chain m a a

infixr 9 >-*
infixr 9 *-*
infixr 9 *->

link = Link
chain = Chain

(>-*) :: Pipe a b m () -> Chain m b c -> Chain m a c
p >-* c = (Link p) `Chain` c

(*-*) :: Chain m a b -> Chain m b c -> Chain m a c
c *-* c' = c `chain` c'

(*->) :: Chain m a b -> Pipe b c m () -> Chain m a c
c *-> p = c `chain` (Link p)

--a >-> b :: PIpe a b m r -> Pipe b c m r -> Pipe a c m r


compileChain ::
  (Monad m, MonadIO m) =>
  (m () -> IO ()) ->
  Chain m a b ->
  IO (Output a, Input b, IO ())
compileChain runner chain = do
  (output, input, cleanup) <- liftIO $ spawn' Unbounded
  (input', closeAsync) <- compileChain' runner chain input
  return (output, input', atomically cleanup >> closeAsync)


compileChain' ::
  (Monad m, MonadIO m) =>
  (m () -> IO ()) ->
  Chain m a b ->
  Input a ->
  IO (Input b, IO ())
compileChain' runner (Link pipe) input = do
  (output, input') <- liftIO $ spawn Unbounded
  asyncHdl <- async $ runner $ runEffect $
    fromInput input >-> pipe >-> toOutput output
  return (input', cancel asyncHdl)

compileChain' runner (Chain chain chain') input = do
  (input', closeAsync)   <- compileChain' runner chain  input
  (input'', closeAsync') <- compileChain' runner chain' input'
  return (input'', closeAsync >> closeAsync')
  --compileChain' chain  input >>= compileChain' chain'

compileChain' runner (Cycle chain) input = do
  -- queue for the chain to pull from
  (leftOutput,  leftInput)  <- liftIO $ spawn Unbounded
  --queue for the chain to push to
  (rightOutput, rightInput) <- liftIO $ spawn Unbounded
  (chainInput, closeAsync) <- compileChain' runner chain leftInput
  let
    --outer loop receives from the given input and starts the inner loop
    outerLoop = do
      recieved <- atomically $ recv input
      case recieved of
        Just value -> do
          atomically $ send leftOutput value
          innerLoop
        Nothing -> return ()
    --inner loop pulls from the chain and decides whether to keep feeding the
    --result back or to push it forward
    innerLoop = do
      recieved <- atomically $ recv chainInput
      (b, action) <- case recieved of
        Just (Left value) -> do
          b <- atomically $ send leftOutput value
          return (b, innerLoop)
        Just (Right value) -> do
          b <- atomically $ send rightOutput value
          return (b, outerLoop)
        Nothing -> return (False, return ())
      if b then action else return ()
  asyncHdl <- liftIO $ async $ outerLoop
  return (rightInput, closeAsync >> cancel asyncHdl)

pump :: (Output (), Input a) -> IO (Maybe a)
pump (output, input) =
  atomically (send output ()) >> atomically (recv input)

pumping :: (Output (), Input a) -> IO [a]
pumping oi = CL.unfoldM (pump oi)

place (output, input) a =
  atomically (send output a) >> fromJust <$> atomically (recv input)


--TODO need cycle, cycleN, parallel chains
--should maybe spawn a second input and only recv from it. then finally recv from outer queue


{-
runStage :: (MonadIO m) =>
  a -> Stage m a a -> RVarT m (Maybe a)
runStage a stage = do
  (output, input, seal) <- liftIO $ spawn' Single
  input' <- stage input
  liftIO $ atomically $ send output a
  result <- liftIO $ atomically $ recv input'
  liftIO $ atomically $ seal
  return result

runStage' :: (MonadIO m) => a -> Stage m a a -> RVarT m a
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

cycleNTimes :: Int -> Stage (SafeT IO) a a -> Stage (SafeT IO) a a
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
stage :: Pipe a b (RVarT (SafeT IO)) () -> Stage (SafeT IO) a b
stage pipe input = do
  (output, input') <- liftIO $ spawn Unbounded
  asyncR $ do
    runEffect $ hoist liftIO (fromInput input) >->
                pipe >->
                hoist liftIO (toOutput output)
    liftIO performGC
  return input'

-- Simple test stage that yields random numbers
simpleStage ::
  (Num a, Distribution Uniform a) =>
  Stage (SafeT IO) a a
simpleStage = stage $ for cat $ \ i -> do
  randomValue <- lift $ uniformT 0 100
  yield (i + randomValue)

throughStage arg stage = do
  (output, input) <- spawn Unbounded
  input' <- rIO $ stage input
  atomically $ send output arg
  atomically $ recv input'

stations n stage input = do
  inputs <- replicateM n (stage input)
  return $ mconcat inputs

station stage input = stations 1 stage input
-}
