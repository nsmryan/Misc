module Evaluation where

import qualified Data.Traversable as T
import qualified Data.Sequence as S
import Data.Random
import Data.Conduit

import System.Log.Logger

import Control.Monad.IO.Class
import Control.Applicative

import Types

{- Evaluaton -}
evaluation :: (Functor m, MonadRandom m) =>
  (a -> m Double) ->
  Pop a ->
  m (Pop (a, Double))
evaluation eval pop = S.zip pop <$> T.mapM eval pop

{- Evaluaton Conduit -}
--evaluationConduit :: 
--  (a -> IO Double) ->
--  Pop a ->
--  HealIO (Pop (a, Double))
evaluationConduit eval pop = do
  yield $ LogResult DEBUG "Evaluation started"
  pop' <- liftIO $ S.zip pop <$> T.mapM eval pop
  yield $ LogResult DEBUG "Evaluation started"
  return pop'

