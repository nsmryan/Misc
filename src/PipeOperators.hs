module PipeOperators where


import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent

import Math.Probable

import qualified Data.Sequence as S

import Control.Monad.Identity
import Control.Monad
import Control.Applicative

import qualified Data.Traversable as T

import Debug.Trace

import Common
import Types
import PointMutation
import Crossover
import Utils
import Selection
import Channels


--element pipe -> sequence pipe
--function -> on Locations, pass others

traced a = trace (show a) a

{- Point Mutation -}
data PMChunk f a = PMPiece (f a)
                 | PMLocation a [Int]
                 | PMFlush deriving (Show, Eq)

pointMutationP :: Double -> Int -> Pipe Ind32 Ind32 R r
pointMutationP prob geneSize =
  probablyDeep prob geneSize >-> pointMutationP' >-> reconstitute

probablyDeep prob locationSize = do
  loc <- lift $ r $ geometric0 prob
  piece <- await
  probablyDeep' prob locationSize loc piece

probablyDeep' prob size loc ind = do
  let len = S.length ind * size 
  if len > loc
    then do
      let (gene, locus) = loc `divMod` size
          (top, bottom) = S.splitAt gene ind
      yield . PMPiece $ top
      loc' <- processPiece prob size locus (S.index bottom 0) []
      probablyDeep' prob size loc' $ S.drop 1 bottom
    else do
      yield . PMPiece $ ind
      yield PMFlush
      ind' <- await
      probablyDeep' prob size (loc - len) ind'


processPiece prob geneSize locusLoc gene ls = do
  loc' <- lift $ r $ geometric0 prob
  let left = geneSize - locusLoc
  if loc' < left
    then do
      let withinGene = loc' + locusLoc
          ls' = (withinGene:ls)
      processPiece prob geneSize withinGene gene ls'
    else do
      yield $ PMLocation gene (locusLoc:ls)
      return $ loc'-left

pointMutationP' = forever $ do
  chunk <- await
  case chunk of
    PMLocation a is -> do
      yield . Location . mutateLocus is $ a
    PMPiece piece -> yield . Piece $ piece
    PMFlush -> yield Flush

testMutation =
  runEffect $ (forever $ yield (S.replicate 1 0)) >->
              pointMutationP 0.5 8 >->
              (await >>= return)

testWith argument pipe = runEffect $ (forever (yield argument)) >-> pipe >-> (await >>= return)
  
--testReconstitute :: IO (S.Seq Integer)
--testReconstitute = runEffect $ producer >-> reconstitute 10 >-> (await >>= return) where
--  producer = forever . yield . S.fromList $ [0..2]

{- Crossover -}
crossoverP prob indLength = 
  for cat (yield . foldInHalf) >->
  probably prob >->
  crossoverP' indLength >->
  reconstitute >->
  for cat (yield . unfoldInHalf)

crossoverP' indLength = forever $ do
  chunk <- await
  case chunk of
    Location pair -> do
      crossPosition <- lift $ r $ intIn (0, indLength-1)
      yield . Location . crossPair crossPosition $ pair
    a -> yield a
  
testCrossover =
  let testPopulation = S.fromList $ fmap S.fromList [sampleInd, reverse sampleInd]
      sampleInd = [0..9]
  in
  testWith testPopulation (crossoverP 1 10)

{- Selection -}
data Tournament a = Tournament a a

tournamentSelectionP :: Int -> Int -> Pipe (S.Seq (a, Double)) (S.Seq a) R r
tournamentSelectionP populationSize tournSize = 
  generateTournamentsP populationSize tournSize >-> competeP >-> collect populationSize

stochasticTournamentSelectionP :: Double -> Int -> Int -> Pipe (S.Seq (a, Double)) (S.Seq a) R r
stochasticTournamentSelectionP gate populationSize tournSize = 
  generateTournamentsP populationSize tournSize >->
  competeStochasticP gate >->
  collect populationSize

generateTournamentsP :: Int -> Int -> Pipe (S.Seq a) (Tournament a) R r
generateTournamentsP populationSize tournSize = forever $ do
  population <- await
  tourns <- lift $ replicateM populationSize (generateTournamentP populationSize population)
  each tourns

generateTournamentP populationLength population = do
  i <- r $ intIn (0, populationLength-1)
  i' <- r $ intIn (0, populationLength-1)
  return $ Tournament (S.index population i) (S.index population i')

competeP :: Monad m => Pipe (Tournament (a, Double)) a m r
competeP = forever $ do
  Tournament a a' <- await
  yield $ fst $ maxBy snd a a'

competeStochasticP :: Double -> Pipe (Tournament (a, Double)) a R r
competeStochasticP gate = forever $ do
  Tournament a a' <- await
  let (high, low) = ensure (a, a')
  choice <- lift $ r $ double
  yield $ fst $ if choice < gate then high else low

testTournament = let population = S.fromList [(S.fromList [0], 5), (S.fromList [1], 10)]
  in testWith population (tournamentSelectionP (S.length population) 2)

{- Utils -}
data Chunk f a = Piece (f a)
               | Location a
               | Flush deriving (Show, Eq)


probably prob = do
  loc <- lift $ r $ geometric0 prob
  piece <- await
  probably' prob loc piece
  
probably' prob loc ind = do
  let len = S.length ind  
  if len > loc
    then do
      let (top, bottom) = S.splitAt loc ind
      yield . Piece $ top
      yield . Location $ S.index bottom 0
      loc' <- lift $ r $ geometric0 prob
      probably' prob loc' $ S.drop 1 bottom
    else do
      yield $ Piece ind
      yield Flush
      ind' <- await
      probably' prob (loc - len) ind'

reconstitute = reconstitute' S.empty

reconstitute' piece = do
  chunk <- await
  case chunk of
    Piece piece' -> do
      reconstitute' $ piece S.>< piece'

    Location a -> do
      reconstitute' $ piece S.|> a

    Flush -> do
      yield piece
      reconstitute' S.empty
 
pairUpP :: Monad m => Pipe a (a, a) m r
pairUpP = forever $ do
  a <- await
  a' <- await
  yield (a, a')
 
unpairP :: Monad m => Pipe (a, a) a m r
unpairP = forever $ do
  (a, a') <- await
  yield a
  yield a'

collect n = forever $ do
  collected <- S.fromList <$> (T.sequence . replicate n $ await)
  yield collected


