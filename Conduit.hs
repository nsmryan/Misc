{-# LANGUAGE  OverloadedStrings #-}
module Conduit where

import qualified Data.Sequence as S
import Data.Conduit
import Data.Void

import System.Log.Logger
import System.Log.Handler
import System.Log.Handler.Simple
import System.Remote.Monitoring
import System.Remote.Counter

import Control.Monad.IO.Class

import Types



--nGenerations ::
--  FilePath ->
--  Int ->
--  ConduitM (Result Pop32) Void IO ()
nGenerations logName gens server = do
  logger <- liftIO $ getLogger logName
  handler <- liftIO $ fileHandler logName DEBUG
  liftIO $ updateGlobalLogger logName (addHandler handler)
  genCounter <- liftIO $ getCounter "Generations" server 
  let loop 0 = return ()
      loop gens = let gens' = pred gens in
        do val <- await
           case val of
             Nothing -> do
               return ()
             Just a -> case a of
               DataResult d -> do
                 liftIO $ inc genCounter
                 if gens' == 0
                 then do
                   liftIO $ print $ show d
                   return ()
                 else loop gens'
               LogResult pri str -> do
                 --liftIO $ logM logName pri str
                 loop gens'
               MonitorResult action -> do
                 --liftIO action
                 loop gens'
    in loop gens
  liftIO $ close handler

