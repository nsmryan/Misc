module Selection where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random
import Data.Function
import Data.List
import qualified Data.Traversable as T
import qualified Data.Foldable as F
--import Data.Conduit

import Control.Monad.IO.Class

import Control.Monad
import Control.Applicative

import Types
import Utils
import UtilsRandom
import Common
import Evaluation


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
  Int -> Pop (Evaled a b) -> R (Pop a)
tournamentSelection size population = let len = S.length population in
  do indices <- replicateM len $ generateTournament size len
     return $ tournamentSelectionPure indices population

{- Stochastic Tournament Selection -}
ensure (ind, ind') = (higher, lower) where
  higher = maxBy snd ind ind'
  lower = minBy snd ind ind'

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
  Prob -> Pop (Evaled a b) -> R (Pop a)
stochasticTournament prob population = 
  let len = S.length population
      genChoices = replicateM len $ fromRange len
      ps = replicateM len (r $ double)
  in do tournaments <- zip3 <$> genChoices <*> genChoices <*> ps
        return $ stochasticTournamentPure prob tournaments population

