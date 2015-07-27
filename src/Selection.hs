{-# LANGUAGE OverloadedStrings #-}
module Selection where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random
import Data.List
import qualified Data.Foldable as F

import Control.Monad
import Control.Applicative

import Pipes
import Pipes.Safe

import Types
import Utils
import UtilsRandom
import Evaluation
import PipeOperators


{- Tournament Selection -}
seqToVect :: S.Seq a -> V.Vector a
seqToVect = V.fromList . F.toList

vectToSeq :: V.Vector a -> S.Seq a
vectToSeq = S.fromList . F.toList

tournamentSelectionPure ::
  [[Int]] -> Pop (Evaled a b) -> Pop a
tournamentSelectionPure competitors population =
  let vectPop = seqToVect population
      len = V.length vectPop
      choices = map (map (vectPop V.!)) competitors
      winners = fmap (maximumBy compareFitnesses) choices
    in genetics . S.fromList $ winners

generateTournament sizeTournament sizePool = do
  replicateM sizeTournament (fromRange sizePool)

tournamentSelection ::
  Int -> Pop (Evaled a b) -> RVarT m (Pop a)
tournamentSelection size population = let len = S.length population in
  do indices <- replicateM len $ generateTournament size len
     return $ tournamentSelectionPure indices population

{- Stochastic Tournament Selection -}
ensure (ind, ind') = (higher, lower) where
  higher = maxBy fitness ind ind'
  lower = minBy fitness ind ind'

choose p pair = let (a, b) = ensure pair in do
  choice <- sample stdUniform
  return $ if choice < p
    then a
    else b

stochasticTournamentPure ::
  Double -> [(Int, Int, Double)] -> Pop (Evaled a b) -> Pop a
stochasticTournamentPure cutoff tournaments population =
  let v = seqToVect population
      len = V.length v
      winners = fight <$> tournaments
      fight (i, i', p) = if p < cutoff then best else worst where
        ind = v V.! i
        ind' = v V.! i'
        best = maxBy fitness ind ind'
        worst = minBy fitness ind ind'
  in
  S.fromList . genetics $ winners

stochasticTournament ::
  Prob -> Pop (Evaled a b) -> RVarT m (Pop a)
stochasticTournament prob population =
  let len = S.length population
      genChoices = replicateM len $ fromRange len
      ps = replicateM len stdUniformT
  in do tournaments <- zip3 <$> genChoices <*> genChoices <*> ps
        return $ stochasticTournamentPure prob tournaments population

{- Pipes and Blocks -}
tournamentBlock :: Block (Pop (Evaled a b)) (Pop a)
tournamentBlock = do
  pr <- lookupConfig (error "population size not provided") "ps"
  tournSize <- lookupConfig (error "Tournament size not provided") "is"
  return $ tournamentSelectionP pr tournSize

tournamentSelectionP
  :: Int -> Int -> Pipe (Pop (Evaled a b)) (Pop a) (RVarT (SafeT IO)) r
tournamentSelectionP populationSize tournSize =
  generateTournamentsP populationSize tournSize >-> competeP >-> collect populationSize

stochasticTournamentSelectionP ::
  Double -> Int -> Int -> Pipe (Pop (Evaled a b)) (Pop a) (RVarT (SafeT IO)) r
stochasticTournamentSelectionP gate populationSize tournSize =
  generateTournamentsP populationSize tournSize >->
  competeStochasticP gate >->
  collect populationSize

generateTournamentsP
  :: Int -> Int -> Pipe (Pop (Evaled a b)) (Tournament (Evaled a b)) (RVarT (SafeT IO)) r
generateTournamentsP populationSize tournSize = forever $ do
  population <- await
  tourns <- lift $ replicateM populationSize (generateTournamentP populationSize population)
  each tourns

generateTournamentP populationLength population = do
  i <- uniformT 0 (populationLength-1)
  i' <- uniformT 0 (populationLength-1)
  return $ Tournament (S.index population i) (S.index population i')

competeP :: Monad m => Pipe (Tournament (Evaled a b)) a m r
competeP = forever $ do
  Tournament a a' <- await
  yield $ genetic . expressed $ maxBy fitness a a'

competeStochasticP :: Double -> Pipe (Tournament (Evaled a b)) a (RVarT (SafeT IO)) r
competeStochasticP gate = forever $ do
  Tournament a a' <- await
  let (high, low) = ensure (a, a')
  choice <- lift $ stdUniformT
  yield $ genetic . expressed $ if choice < gate then high else low

