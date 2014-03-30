{-# LANGUAGE  OverloadedStrings #-}
module Conduit where

import qualified Data.Sequence as S
import Data.Conduit
import Data.Conduit.TMChan
import Data.Void

import System.Remote.Monitoring
import System.Remote.Counter

import Control.Monad.IO.Class
import Control.Concurrent.STM.TBMChan

import GHC.Conc

import Types


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

