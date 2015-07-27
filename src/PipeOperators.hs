{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module PipeOperators where


import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import Pipes.Safe as Safe
import qualified Pipes.ByteString as PB

--import Math.Probable

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Configurator as C
import Data.List
import Data.String
import Data.Random

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Concurrent.Async
import Control.Monad.Trans.State

import Debug.Trace

import System.IO

import Common
import Types
import Utils
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
elitismBlock ::
  (Eq a) =>
  Int ->
  Pipe (Pop (Evaled a b)) (Pop a) (RVarT (Safe.SafeT IO)) () ->
  Block (Pop (Evaled a b)) (Pop a)
elitismBlock k pipe =
  return $ elitismP k pipe

elitismP ::
  (Eq a) =>
  Int ->
  Pipe (Pop (Evaled a b)) (Pop a) (RVarT (Safe.SafeT IO)) () ->
  Pipe (Pop (Evaled a b)) (Pop a) (RVarT (Safe.SafeT IO)) ()
elitismP k pipe = do
  hoist (flip evalStateT S.empty)
        (storeFittest k >-> hoist lift pipe >-> ensureFittest)

storeFittest ::
  Int ->
  Pipe (Pop (Evaled a b)) (Pop (Evaled a b)) (StateT (Pop a) (RVarT (Safe.SafeT IO))) ()
storeFittest  k = forever $ do
  pop <- await
  lift $ put $ fmap (genetic . expressed) $ kFittest k pop
  yield pop

ensureFittest ::
  (Eq a) =>
  Pipe (Pop a) (Pop a) (StateT (Pop a) (RVarT (Safe.SafeT IO))) ()
ensureFittest = forever $ do
  pop <- await
  elitest <- lift get
  yield $ ensureElems pop elitest

elitism ::
  Int ->
  (Pop (Evaled a b) -> RVarT m (Pop a)) ->
  Pop (Evaled a b) ->
  RVarT m (Pop a)
elitism k select pop = do
  let (elite, common) = S.splitAt k $ S.sortBy compareFitnesses pop
  selected <- select common
  return (fmap (genetic . expressed) elite S.>< selected)

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
  return $ logTo "locus.log" $ logAvgLocus' bitsUsed (is :: Int)

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
  return $ logTo "diversity.log" $ logWordDiversity' bitsUsed

logWordDiversity' :: Int -> Pipe Pop32 String (SafeT IO) r
logWordDiversity' bitsUsed = do
  yield "diversity"
  loop where
    loop = do
      pop <- await
      let diverse = wordDiversity bitsUsed pop
      yield $ show diverse ++ "\n"
      loop

--logTo ::
--  (MonadSafe m, MonadIO m, Base m ~ IO) =>
--  FilePath -> Pipe a String m () -> Pipe a a m ()
logTo ::
  FilePath ->
  Pipe a String (SafeT IO) () -> Pipe a a (RVarT (SafeT IO)) ()
logTo fileName pipe =
  hoist lift $ Safe.bracket (liftIO $ openFile fileName WriteMode)
               (liftIO . hClose)
               (\h -> P.tee $ pipe >-> P.map fromString >-> P.toHandle h)

{- Flow Control -}
gensBlock :: Block a (Either a a)
gensBlock = do
  gens <- lookupConfig (error "generations not provided") "gens"
  return (go gens) where
    go :: Int -> Pipe a (Either a a) (RVarT m) ()
    go n = do
      a <- await
      yield $ if n == 0 then Right a else Left a
      go (n-1)

asyncLog ::
  (MonadIO m) =>
  FilePath -> Pipe a String IO () -> Pipe a a m ()
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

onElementsP :: (Monad m) => Int -> Pipe a a m r -> Pipe (S.Seq a) (S.Seq a) m r
onElementsP size pipe = forever $ do
  for cat each >-> pipe >-> collect size

