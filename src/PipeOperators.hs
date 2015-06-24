{-# LANGUAGE OverloadedStrings #-}
module PipeOperators where


import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent

import Math.Probable

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Configurator as C

import Control.Monad.Identity
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Reader

import Debug.Trace

import Common
import Types
import PointMutation
import Crossover
import Utils
import Selection
import Channels
import UtilsRandom
import Rotation


--element pipe -> sequence pipe
--function -> on Locations, pass others

traced a = trace (show a) a

{- Utilities -}

lookupConfig def nam = do
  config <- ask 
  lift $ C.lookupDefault def config nam

--locusP :: (Monad m, Foldable f) => Pipe (f a) a m r
locusP prob size pipe =
  for cat each >-> locusP' prob size pipe >-> collect size

locusP' prob size pipe =
  probablyDeep prob size >-> pipe >-> reconstitute

{- Point Mutation -}
data PMChunk f a = PMPiece (f a)
                 | PMLocation a [Int]
                 | PMFlush deriving (Show, Eq)


pmBlock :: Block Pop32 Pop32
pmBlock = do
  pm <- lookupConfig 0.001 "pm"
  ps <- lookupConfig (error "population size not provided") "ps"
  geneSize <- lookupConfig (error "gene size not provided") "bitsUsed"
  return $ pmPopulationP ps pm geneSize


pmPopulationP ps pm geneSize = --locusP pm geneSize (pointMutationP pm geneSize)
  (for cat each) >-> (pointMutationP pm geneSize) >-> (collect ps)

pointMutationP :: Double -> Int -> Pipe Ind32 Ind32 R r
pointMutationP prob geneSize = --locusP prob geneSize pointMutationP'
  probablyDeep prob geneSize >-> pointMutationP' >-> reconstitute


--data LayerChunk f a = LayerChunk (f a)
--                    | LayerLocation a
--                    | LayerEnd deriving (show)
--
--selectInLayer size = do
--  undefined
--
--layeredMapR prob ss = 
--  let layerSizes = init $ scanr (*) 1 ss
--      selectionPipe = foldl (>->) id $ map selectInLayer layerSizes
--  in
--    do
--      loc <- lift $ r $ geometric0 prob
--      selectionPipe loc


--probablyDeep prob locationSize innerPipe = forever $ do
--  loc <- lift $ r $ geometric0 prob
--  let (skips, inner) = loc `divMod` locationSize
--  pass skips
--  piece <- await
--  innerPipe piece

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
  
{- Crossover -}
crossoverIndividualsP prob indLength =
  pairUpP >->
  withP prob >->
  whenChosen (crossoverIndividualP indLength) >->
  unpairP

crossoverIndividualP indLength pair = do
  point <- r $ intIn (0, indLength-1)
  return $ crossPair point pair


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

crossoverMultipointP prob indLength points = 
  for cat (yield . foldInHalf) >->
  probably prob >->
  crossoverMultiP' indLength points >->
  reconstitute >->
  for cat (yield . unfoldInHalf)

crossoverMultiP' indLength points = do
  chunk <- await
  case chunk of
    Location pair -> do
      crossPositions <- lift $ r $ replicateM points $ intIn (0, indLength-1)
      yield . Location . multicrossPair crossPositions $ pair
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

{- Rotation -}
rotationP prob indLength =
  withP prob >->
  whenChosen (rotateOp indLength)

rotateOp indLength individual = do
  rotationPoint <- r $ intIn (0, indLength-1)
  return $ rotateIndividual rotationPoint individual

{- Elitism -}
--K-Elitism specialized for the k = 1 case
fittestIndividual = F.maximumBy compareFitness

keepFittestP :: (Eq a) =>
  Pipe (S.Seq (S.Seq a, Double)) (S.Seq (S.Seq a, Double)) R r
keepFittestP = do
  population <- await
  keepFittestP' $ fittestIndividual population

keepFittestP' :: (Eq a) =>
  (S.Seq a, Double) ->
  Pipe (S.Seq (S.Seq a, Double)) (S.Seq (S.Seq a, Double)) R r
keepFittestP' fittest = do
  population <- await
  let newFittest = fittestIndividual population
  if fittest `F.elem` population
    then
      yield population
    else
      yield $ fittest S.<| (S.drop 1 population)
  keepFittestP' newFittest

compareFitness :: (Ord b) => (a, b) -> (a, b) -> Ordering
compareFitness = compare `on` snd

sortByFitness = S.sortBy compareFitness

kFittest k pop = S.take k $ sortByFitness pop

elitismP :: (Eq a) =>
  Int ->
  Pipe (S.Seq (S.Seq a, Double)) (S.Seq (S.Seq a, Double)) R r
elitismP 1 = keepFittestP
elitismP k = do
  population <- await
  keepK k $ kFittest k population

--ensure an individual is in a population
ensureElem :: (Eq a) => S.Seq a -> a -> S.Seq a
ensureElem population individual =
  if individual `F.elem` population
    then population
    else individual S.<| (S.take ((S.length population) - 1) population)

keepK :: (Eq a) =>
  Int ->
  (S.Seq (S.Seq a, Double)) ->
  Pipe (S.Seq (S.Seq a, Double)) (S.Seq (S.Seq a, Double)) R r
keepK k best = do
  population <- await
  let newBest = kFittest k population
  yield $ F.foldl ensureElem population best
  keepK k newBest

{- Utils -}
data Chunk f a = Piece (f a)
               | Location a
               | Flush deriving (Show, Eq)


data Choose a = Chosen a | NotChosen a

withP prob = forever $ do
  a <- await
  p <- lift $ r $ double
  yield $ if p < prob
    then Chosen a
    else NotChosen a

whenChosen f = forever $ do 
  value <- await
  case value of
    Chosen a -> do
      a' <- lift $ f a
      yield a'
    NotChosen a -> yield a

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


