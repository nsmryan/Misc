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

tournamentSelectionConduit :: 
  Int ->
  Conduit (Pop (Ind a, Double)) IO (Pop (Ind a))
tournamentSelectionConduit size = awaitForever (liftIO . tournamentSelection size)

{- Stochastic Tournament Selection -}
ensure (ind, ind') = (higher, lower) where
  higher = maxBy snd ind ind'
  lower = minBy snd ind ind'

choose p pair = let (a, b) = ensure pair in do
  choice <- sample stdUniform
  return $ if choice < p
    then a
    else b

--TODO split out pure and impure parts
stochasticTournamentPure ::
  [Bool] -> Pop (a, Double) -> Pop a
stochasticTournamentPure winners population = undefined

stochasticTournament :: (MonadRandom m, Applicative m) =>
  Prob -> Pop (a, Double) -> m (Pop a)
stochasticTournament prob population = do
  let vectPop = seqToVect population
      len = V.length vectPop
      genChoices = replicateM len $ fromRange len
  choices0 <- genChoices
  choices1 <- genChoices
  let chosen0 = fmap (vectPop V.!) choices0
  let chosen1 = fmap (vectPop V.!) choices1
  winners <- T.traverse (choose prob) (zip chosen0 chosen1)
  return . fmap fst . S.fromList $ winners

{- Conduit -}
stochasticTournamentConduit ::
  Prob ->
  Conduit (Pop (a, Double)) IO (Pop a)
stochasticTournamentConduit prob = awaitForever (liftIO . stochasticTournament prob)

