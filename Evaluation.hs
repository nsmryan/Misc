module Evaluation where

import qualified Data.Traversable as T
import qualified Data.Sequence as S
import Data.Random

import Control.Monad.IO.Class
import Control.Applicative

import Types


{- Evaluaton -}
evaluation :: (Functor m, MonadRandom m) =>
  (a -> m Double) ->
  Pop a ->
  m (Pop (a, Double))
evaluation eval pop = S.zip pop <$> T.mapM eval pop

