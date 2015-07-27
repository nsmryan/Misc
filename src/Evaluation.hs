{-# LANGUAGE OverloadedStrings #-}
module Evaluation where

import qualified Data.Traversable as T
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Control.Applicative

import qualified Pipes.Prelude as P
import Pipes
import Pipes.Safe

import Types
import Common
import PipeOperators
import Utils


evaled individual fitness = Evaled individual fitness


{- Evaluaton -}
evaluation ::
  (b -> Double) ->
  Pop (Expressed a b) ->
  (Pop (Evaled a b))
evaluation eval pop = fmap eval' pop where
  eval' expr = Evaled expr (eval . expression $ expr)

evaluationM eval pop = T.mapM (\ind -> Evaled ind <$> (eval $ expression ind)) pop

expresseds  is = fmap expressed is
expressions is = fmap (expression . expressed) is
genetics    is = fmap (genetic . expressed) is

--runExprTree :: ExprTree a -> a
--runExprTree (ExprTree root children) =
--  head . program root . map runExprTree $ children
--
--prefixExprTree :: ExprTree a -> String
--prefixExprTree (ExprTree root children) =
--  "(" ++ name root ++ " " ++ (concatMap prefixExprTree children) ++ ")"

{- Expression -}
expressionBlock :: (a -> b) -> Block (Pop a) (Pop (Expressed a b))
expressionBlock f = return $ expressPopWithP f

expressPopWithP f = P.map . expressPopWith $ f

expressPopWith f = fmap (expressWith f)

expressWith f a = Expressed a (f a)


{- Fitness -}
simpleFitnessBlock :: (b -> Double) -> Block (Pop (Expressed a b)) (Pop (Evaled a b))
simpleFitnessBlock fitnessFunction = return $ P.map $ evaluation fitnessFunction
      --return $ for cat each >-> (for cat $ \ ind -> yield $ evaluationFunction ind) >-> collect ps

fitnessBlock :: (Expressed a b -> Double) -> Block (Pop (Expressed a b)) (Pop (Evaled a b))
fitnessBlock fitnessFunction = return $ P.map $ fmap (uncurry Evaled . (id &&& fitnessFunction))

--TODO should get a directory to start logging into.
logFitness ::
  Block (Pop (Evaled (Ind a) b)) (Pop (Evaled (Ind a) b))
logFitness =
  loggingBlock $ return $ logTo "fitness.log" $ logFitness'

logFitness' ::
  Pipe (Pop (Evaled (Ind a) b)) String (SafeT IO) r
logFitness' = do
  yield "average fitness, best fitness"
  loop where
    loop = do
      pop <- await
      let best = fitness $ fittestIndividual pop
      let fits = fmap fitness pop
      let avg = F.sum fits / (fromIntegral $ S.length fits)
      yield $ (show avg) ++ ", " ++ (show best)
      loop

