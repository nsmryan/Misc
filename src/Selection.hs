module Selection where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random
import Data.Function
import Data.List
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Conduit

import Control.Monad.IO.Class

import Control.Monad
import Control.Applicative

import Types
import Utils
import UtilsRandom


{- Tournament Selection -}
seqToVect :: S.Seq a -> V.Vector a
seqToVect = V.fromList . F.toList

vectToSeq :: V.Vector a -> S.Seq a
vectToSeq = S.fromList . F.toList

tournamentSelectionPure :: 
  [[Int]] -> Pop (Ind a, Double) -> Pop (Ind a)
tournamentSelectionPure competitors population =
  let vectPop = seqToVect population
      len = V.length vectPop
      choices = map (map (vectPop V.!)) competitors 
      winners = fmap (maximumBy (compare `on` snd)) choices
    in fmap fst . S.fromList $ winners

generateTournament sizeTournament sizePool = do
  replicateM sizeTournament (fromRange sizePool)

tournamentSelection :: (MonadRandom m) =>
  Int -> Pop (Ind a, Double) -> m (Pop (Ind a))
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
  Double -> [(Int, Int, Double)] -> Pop (a, Double) -> Pop a
stochasticTournamentPure cutoff tournaments population = 
  let v = seqToVect population
      len = V.length v
      winners = fight <$> tournaments
      fight (i, i', p) = if p < cutoff then best else worst where
        ind = v V.! i
        ind' = v V.! i'
        best = maxBy snd ind ind'
        worst = minBy snd ind ind'
  in
  S.fromList . map fst $ winners

stochasticTournament :: (MonadRandom m, Applicative m) =>
  Prob -> Pop (a, Double) -> m (Pop a)
stochasticTournament prob population = 
  let len = S.length population
      genChoices = replicateM len $ fromRange len
      ps = replicateM len (sample stdUniform)
  in do tournaments <- zip3 <$> genChoices <*> genChoices <*> ps
        return $ stochasticTournamentPure prob tournaments population

