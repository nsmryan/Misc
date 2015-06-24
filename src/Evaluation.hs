module Evaluation where

import qualified Data.Traversable as T
import qualified Data.Sequence as S
import Data.Random

import Control.Monad.IO.Class
import Control.Applicative

import Types
import Common
import UtilsRandom


                    
evaled individual fitness = Evaled individual fitness

evalInd :: (b -> R Double) -> (Expressed a b) -> R Double
evalInd eval (Expressed _ ind) = eval ind

{- Evaluaton -}
evaluation ::
  (Expressed a b -> R Double) ->
  Pop (Expressed a b) ->
  R (Pop (Evaled a b))
evaluation eval pop = S.zipWith Evaled pop <$> T.mapM eval pop

{-
evaluationM ::
  (a -> Double) ->
  Pop a ->
  R (Pop (Evaled a))
-}
evaluationM eval pop = S.zip pop <$> fmap eval pop

expresseds  is = fmap expressed is
expressions is = fmap (expression . expressed) is
genetics    is = fmap (genetic . expressed) is

compareFitnesses = compare `on` fitness

runExprTree :: ExprTree a -> a
runExprTree (ExprTree root children) =
  head . program root . map runExprTree $ children

prefixExprTree :: ExprTree a -> String
prefixExprTree (ExprTree root children) =
  "(" ++ name root ++ " " ++ (concatMap prefixExprTree children) ++ ")"

