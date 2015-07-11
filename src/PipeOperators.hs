{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module PipeOperators where


import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import qualified Pipes.Safe.Prelude as PS
import Pipes.Safe
import qualified Pipes.ByteString as PB

--import Math.Probable

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Configurator as C
import Data.Maybe
import Data.List
import Data.String
import Data.Random
import qualified Data.ByteString.Lazy as BL

import Control.Monad.Identity
import Control.Monad
import Control.Monad.Catch
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Concurrent.Async

import Debug.Trace

import System.IO

import Common
import Types
import Utils
import Channels
import UtilsRandom


--TODO Should use Evaled for fitness pipes

--element pipe -> sequence pipe
--function -> on Locations, pass others

traced a = trace (show a) a

{- Utilities -}


runBlock config block = runReaderT block config

lookupConfig def nam = do
  config <- ask
  lift $ C.lookupDefault def config nam

-- lookups for commonly needed values
lookForPM   = lookupConfig (0.001 :: Double) "pm"
lookForPR   = lookupConfig (0.6   :: Double) "pr"
lookForPC   = lookupConfig (0.6   :: Double) "pc1"
lookForPC2  = lookupConfig (0.6   :: Double) "pc"
lookForPT   = lookupConfig (0.75  :: Double) "pt"
lookForGens = lookupConfig (1000  :: Int)    "gens"
lookForPS   = lookupConfig (50    :: Int)    "ps"
lookForIS   = lookupConfig (200   :: Int)    "is"

{- Elitism -}
--K-Elitism specialized for the k = 1 case
fittestIndividual = F.maximumBy compareFitness

keepFittestP :: (Eq (Evaled (Ind a) b)) =>
  Pipe (Pop (Evaled (Ind a) b)) (Pop (Evaled (Ind a) b)) (RVarT m) r
keepFittestP = do
  population <- await
  keepFittestP' $ fittestIndividual population

keepFittestP' :: (Eq (Evaled (Ind a) b)) =>
  (Evaled (Ind a) b) ->
  Pipe (Pop (Evaled (Ind a) b)) (Pop (Evaled (Ind a) b)) (RVarT m) r
keepFittestP' fittest = do
  population <- await
  let newFittest = fittestIndividual population
  if fittest `F.elem` population
    then
      yield population
    else
      yield $ fittest S.<| (S.drop 1 population)
  keepFittestP' newFittest

compareFitness :: (Evaled a b) -> (Evaled a b) -> Ordering
compareFitness = compare `on` fitness

sortByFitness = S.sortBy compareFitness

kFittest k pop = S.take k $ sortByFitness pop

elitismP :: (Eq a, Eq b) =>
  Int ->
  Pipe (Pop (Evaled (Ind a) b)) (Pop (Evaled (Ind a) b)) (RVarT m) r
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

keepK :: (Eq a, Eq b) =>
  Int ->
  (Pop (Evaled (Ind a) b)) ->
  Pipe (Pop (Evaled (Ind a) b)) (Pop (Evaled (Ind a) b)) (RVarT m) r
keepK k best = do
  population <- await
  let newBest = kFittest k population
  yield $ F.foldl ensureElem population best
  keepK k newBest

{- Logging -}

loggingBlock :: Block a a -> Block a a
loggingBlock block = do
  loggingFlag <- lookupConfig False "logging"
  if loggingFlag
    then block
    else return cat

{- Diversity -}
logAvgLocus :: Block Pop32 Pop32
logAvgLocus = loggingBlock $ do
  bitsUsed <- lookupConfig 1 "bitsUsed"
  is <- lookupConfig (error "individual size not provided") "is"
  return $ hoist lift $ logTo "locus.log" $ logAvgLocus' bitsUsed (is :: Int)

logAvgLocus' :: Int -> Int -> Pipe Pop32 String (SafeT IO) r
logAvgLocus' bitsUsed is = do
  yield "locuses"
  loop where
    loop = do
      pop <- await
      let popLists = F.toList $ fmap F.toList pop
      let expandedBits = (map . map) (expandBits bitsUsed) popLists
      let pop' = map concat expandedBits
      let sums = map sum $ transpose pop'
      let avgs = map ((/fromIntegral is) . fromIntegral) sums
      yield $ (++ "\n") $ intercalate "," $ map show avgs
      loop

logWordDiversity :: Block Pop32 Pop32
logWordDiversity = loggingBlock $ do
  bitsUsed <- lookupConfig 1 "bitsUsed"
  return $ hoist lift $ logTo "diversity.log" $ logWordDiversity' bitsUsed

logWordDiversity' :: Int -> Pipe Pop32 String (SafeT IO) r
logWordDiversity' bitsUsed = do
  yield "diversity"
  loop where
    loop = do
      pop <- await
      let diverse = wordDiversity bitsUsed pop
      yield $ show diverse ++ "\n"
      loop

logTo :: (MonadSafe m, MonadIO m) => FilePath -> Pipe a String m () -> Pipe a a m ()
logTo fileName pipe = do
  --hd <- liftIO $ openFile fileName WriteMode
  P.tee $ pipe >-> P.map fromString >-> PS.writeFile fileName

{- Flow Control -}
gensBlock :: Block a a
gensBlock = do
  gens <- lookupConfig (error "generations not provided") "gens"
  return (go gens) where
    go :: Int -> Pipe a a (RVarT m) ()
    go 0 = return ()
    go n = do
      a <- await
      yield a
      go (n-1)

asyncLog :: (MonadIO m) => FilePath -> Pipe a String IO () -> Pipe a a m ()
asyncLog fileName pipe = do
  (output, input) <- liftIO $ spawn Unbounded
  let outPipe = toOutput output
  liftIO $ async $ return $ withFile fileName WriteMode $ \hd -> runEffect $ (fromInput input) >-> pipe >-> P.map fromString >-> PB.toHandle hd
  P.tee $ outPipe

--asyncSafeTee :: (MonadIO m) => Consumer a (SafeT IO) () -> Pipe a a m ()
--asyncSafeTee consumer = do
--  (output, input) <- liftIO $ spawn Unbounded
--  let outPipe = toOutput output
--  liftIO $ runSafeT $ bracket (async $ return $ runEffect $ (fromInput input) >-> consumer) wait return
--  P.tee $ outPipe

cycleWith :: a -> Pipe a a (RVarT (SafeT IO)) () -> IO a
cycleWith a pipe = do
  (output, input) <- liftIO $ spawn (Bounded 1)
  (finalOutput, finalInput) <- liftIO $ spawn (Newest 1)
  thread <- async $ runSafeT $ rIO $ runEffect $ (fromInput input) >-> pipe >-> toOutput (output <> finalOutput)
  atomically $ send output a
  wait thread
  (Just result) <- liftIO $ atomically $ recv finalInput
  return result

{- Utils -}

withP prob = forever $ do
  a <- await
  p <- lift $ stdUniformT
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
  loc <- lift $ geo0 prob
  piece <- await
  probably' prob loc piece

probably' prob loc ind = do
  let len = S.length ind
  if len > loc
    then do
      let (top, bottom) = S.splitAt loc ind
      yield . Piece $ top
      yield . Location $ S.index bottom 0
      loc' <- lift $ geo0 prob
      probably' prob loc' $ S.drop 1 bottom
    else do
      yield $ Piece ind
      yield Flush
      ind' <- await
      probably' prob (loc - len) ind'

reconstitute :: (Monad m) => Pipe (Chunk S.Seq a) (S.Seq a) m r
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

onPairsP pipe = pairUpP >-> pipe >-> unpairP

collect n = forever $ do
  collected <- S.fromList <$> (T.sequence . replicate n $ await)
  yield collected

onElementsP :: (Monad m) => Pipe a a m () -> Pipe (S.Seq a) (S.Seq a) m ()
onElementsP pipe = forever $ do
  as <- await
  each as >-> pipe >-> collect (S.length as)

