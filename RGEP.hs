{-# LANGUAGE  FlexibleContexts #-}
module RGEP where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Monoid
import Data.List
import Data.List.Split
import Data.Bits
import Data.Function
import Data.Random
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Text.Printf

import Debug.Trace

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Primitive
import Control.Monad.Primitive.Class
import Control.Monad.Trans.State.Lazy

import UtilsRandom


--problem with randomly- will skip rest of lowest layer
--  item. should keep calling until done on that layer?
--  or is there a more general solution?
--finish splitting randomly into monadic and regular
--may be helpful to use conduit to implement point mutation
--split pure and impure aspects of operators
--add conduit aspect
--add system monitoring
--add logging
--add config file
d a = traceShow a a 

type Prob = Double

{- Operators -}
type Sym = String

data Arity = Arity { inputs :: Int
                   , outputs :: Int
                   } deriving (Show, Eq)

instance Monoid Arity where
  (Arity ins outs) `mappend` (Arity ins' outs') =
    Arity (ins   + max 0 (ins' - outs))
          (outs' + max 0 (outs - ins'))
  mempty = Arity 0 0
 
type StackProg a = [a] -> [a]

data Op a = Op { name :: Sym
               , arity :: Arity
               , program :: StackProg a
               }

instance Show (Op a) where
  show = name

mkOp1 name f = Op name (Arity 1 1) (onHead f) where onHead f (a:as) = f a : as
mkOp2 name f = Op name (Arity 2 1) (firstTwo f) where firstTwo f (a:a':as) = a `f` a' : as
mkTerm name v = Op name (Arity 0 1) (v:)

-- Plain arithmatic operations
plusOp = mkOp2 "+"    (+)
minusOp = mkOp2 "-"   (-)
timesOp = mkOp2 "*"   (*)
divOp = mkOp2 "/"     (/)
incOp = mkOp1 "inc"   (1.0 +)
zeroTerm :: Op Double
zeroTerm = mkTerm "0" (0.0)
oneTerm = mkTerm "1"  (1.0)
twoTerm = mkTerm "2"  (2.0)

-- Stack operations
dup  = Op "dup"  (Arity 1 2) (uncurry (:) . (head &&& id))
drop = Op "drop" (Arity 1 0) tail
swap = Op "swap" (Arity 2 2) $ \ (a:a':as) -> a':a:as
tuck = Op "tuck" (Arity 2 3) $ \ (a:a':as) -> (a:a':a:as)

-- Polynomials
-- Expressions
--

filterUnderflows ops = map fst . filter (snd) . zip ops . snd . mapAccumL composeEffects mempty $ ops
composeEffects arr op = if outputs arr < inputs (arity op) then (arr, False) else (arr <> arity op, True)

progArity = Arity 0 1 --from [] to [a]

dec a = a-1

onlyRunnable ops = take (lastFullProg ops) ops where
  incrProgs = scanl1 (<>) . map arity
  fullProg = findIndices (== progArity) 
  getLength as = 1 + last as
  lastFullProg = getLength . fullProg . incrProgs

cleanProg = onlyRunnable . filterUnderflows

stackEffect :: [Op a] -> Arity
stackEffect = mconcat . map arity

runOpsUnsafe ops = foldl (.) id (map program . reverse $ ops) []
runOps = runOpsUnsafe . cleanProg

runProgram prog = do
  let filtered = filterUnderflows prog 
  guard . not . null $ filtered
  let runnable = onlyRunnable filtered
  guard . not . null $ runnable
  return $ runOpsUnsafe runnable

runProgramWithDefault :: a -> [Op a] -> a
runProgramWithDefault def prog = maybe def head (runProgram prog)

testStacks = do
      let prog = [oneTerm, twoTerm, plusOp, dup, timesOp, twoTerm] 
      let progUnderflow = [oneTerm, twoTerm, plusOp, timesOp] 
      print $ "safe program"
      print $ map name prog
      print $ "safe program, filtered"
      print $ map name $ filterUnderflows prog
      print $ "safe program, cleaned"
      print $ map name $ cleanProg prog
      print $ "safe program, run unsafe"
      print $ runOpsUnsafe prog
      print $ "safe program, run safe"
      print $ runOps prog
      print $ "unsafe program"
      print $ map name progUnderflow
      print $ "unsafe program, filtered"
      print $ map name $ filterUnderflows progUnderflow
      print $ "unsafe program, cleaned"
      print $ map name $ cleanProg progUnderflow
      print $ "unsafe program, safely"
      print $ runOps progUnderflow
      print $ "unsafe program, unsafely"
      print $ runOpsUnsafe progUnderflow

{- Express raw bits -}

bitsUsed ops = let (terms, nonterms) = splitSymbols ops in
  if length terms > 0 && length nonterms > 0
    then 1 + max (bitsRequired (length terms)) (bitsRequired (length nonterms))
    else error "There must be at least one terminal symbol and one nonterminal symbol"

bitsRequired n = ceiling $ logBase 2 (fromIntegral n)

splitSymbols ops = (filter ((==0) . inputs . arity) ops, filter ((/=0) . inputs . arity) ops)

type Decoder a = Word32 -> Op a

decode :: [Op a] -> Decoder a
decode ops = uncurry decodeSymbols $ splitSymbols ops

decodeSymbols :: [Op a] -> [Op a] -> Decoder a
decodeSymbols terms nonterms = let
  termsV = V.fromList terms
  nontermsV = V.fromList nonterms
  in \ w -> let index = fromIntegral $ w `shiftR` 1
                symV = if testBit w 0 then nontermsV else termsV
                in symV V.! (index `mod` V.length symV)

testDecode = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
  let termIndices = map (`shiftL` 1) [0..5]
  let nontermIndices = map ((`setBit` 0) . (`shiftL` 1)) [0..5]
  print termIndices
  print nontermIndices
  putStrLn ""
  print $ map (decode ops) termIndices
  print $ map (decode ops) nontermIndices

{- Generate Random population -}
type Ind a = S.Seq a
type Pop a = S.Seq (Ind a)

type Ind32 = S.Seq Word32
type Pop32 = S.Seq Ind32

type IndR = S.Seq Double
type PopR = S.Seq IndR

type IndBits = Int
type PopBits = S.Seq IndBits

stdIndividual :: (Distribution StdUniform a, MonadRandom m) =>
  Int -> m (Ind a)
stdIndividual is = S.replicateM is $ sample stdUniform

stdPopulation :: (Distribution StdUniform a, MonadRandom m) =>
  Int -> Int -> m (Pop a)
stdPopulation ps is = S.replicateM ps $ stdIndividual is

ind32 :: (MonadRandom m) => Int -> Int -> m Ind32
ind32 is numBits = S.replicateM is $ sample $ uniform 0 bits where
  bits = (2 ^ (fromIntegral numBits)) - 1

pop32 :: (MonadRandom m) => Int -> Int -> Int -> m Pop32
pop32 ps is bits = S.replicateM ps $ ind32 is bits

indR :: (MonadRandom m) => Int -> m IndR
indR is = S.replicateM is $ sample stdUniform

popR :: (MonadRandom m) => Int -> Int -> m PopR
popR ps is = S.replicateM ps $ indR is

indBits :: (MonadRandom m) => Int -> m IndBits
indBits cap = fromRange cap

popBits :: (MonadRandom m) => Int -> Int -> m PopBits
popBits len cap = S.replicateM len $ indBits cap

{- Incremental Population Based Learning -}
minBy f a b = if f a < f b then a else b
maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = if f a >= f b then a else b

timesM :: (Monad m) => Int -> a -> (a -> m a) -> m a
timesM i a m = foldl (>=>) return (replicate i m) $ a

genInd :: (MonadRandom m, T.Traversable t) =>
  t Prob -> m (t Bool)
genInd = T.mapM $ \ p -> do
  p' <- sample stdUniform
  return $ p' > p

b2d :: Bool -> Double
b2d True = 1.0
b2d False = 0.0

adjustProb learn neglearn p minBit maxBit =
  if minBit == maxBit
    then (p * (1 - learn))  + (b2d minBit * learn)
    else (p * (1 - learn2)) + (b2d minBit * learn2) where
      learn2 = learn + neglearn

mutIBPL probs' mutRate mutShift = S.zipWith3 mut probs' <$> bs <*> ps where
  len = S.length probs'
  ps = S.replicateM len $ sample $ uniform 0 (1.0 :: Double)
  bs = S.replicateM len $ sample $ uniform 0 (1.0 :: Double)
  mut p b p' = let b' = fromIntegral . round $ b in
    if p' < mutRate
      then (p * (1 - mutShift)) + (b' * mutShift) 
      else p

pbil' ps learn neglearn mutRate mutShift express evaluate (best, probs) = do
  inds <- S.replicateM ps $ genInd probs
  let inds' = express <$> inds
  let evaled = S.zip inds $ evaluate <$> inds'
  let minInd = F.minimumBy (compare `on` snd) evaled
  let maxInd = F.maximumBy (compare `on` snd) evaled
  let best' = minBy snd best minInd
  let probs' = S.zipWith3 (adjustProb learn neglearn) probs (fst minInd) (fst maxInd)
  probs'' <- mutIBPL probs' mutRate mutShift
  return (best', probs'')

pbil ps is gens learn neglearn mutRate mutShift express eval = do
  let probs = S.replicate is 0.5
  initialBest <- genInd probs
  ((finalBest, fit), probs) <- timesM gens ((initialBest, eval $ express initialBest), probs) $ pbil' ps learn neglearn mutRate mutShift express eval
  return (express finalBest, fit, probs)

maxValue :: (Enum a, Functor f, F.Foldable f) => f a -> Double
maxValue = (0.0 -) . F.sum . fmap (fromIntegral . fromEnum)

testPBIL = do
  let ps = 20
  let is = 100
  let gens = 1000
  (ind, fit,  probs) <- runRandIO $ pbil ps is gens 0.05 0.01 0.2 0.05 id maxValue
  print $ negate fit

{- RGEP PBIL -}
b2i True = 1
b2i False = 0

pack :: [Bool] -> Word32
pack bs = foldl (\ w b -> (w `shiftL` 1) .|. b2i b) 0 bs

collect ::  Int -> S.Seq Bool -> [Word32]
collect n bs = pack <$> (splitEvery n $ F.toList bs)

rgepPBIL ops ps is gens learn neglearn mutRate mutShift eval =
  pbil ps bs gens learn neglearn mutRate mutShift (collect bits) eval where
    bs = bits * is
    bits = bitsUsed ops

testRGEPPBIL = do
  let ps = 50
  let is = 20
  let gens = 1000
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  let eval = (0.0 -) . runProgramWithDefault 0.0 . fmap decoder
  (ind, fit, probs) <- runRandIO $ rgepPBIL ops ps is gens 0.1 0.075 0.02 0.05 eval
  print probs
  putStrLn ""
  print $ fmap name . cleanProg . fmap decoder $ ind
  putStrLn ""
  print $ negate fit

{- Random Mutation Hill Climbing RGEP -}

repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

skipping :: (Monad m) => Int -> StateT [Int] m Int
skipping n = do
  (a:as) <- get
  if a == 0
    then return 0
    else do 
      let (skips, m) = divMod a n
      put (m:as)
      return $! skips

takeUnder n = takeUnder' n 0 where
  takeUnder' n acc =
    do as <- get
       case as of
         [] -> return []
         a:as' -> do
           let acc' = a + acc
           if acc' >= n
             then do
               put $ acc' - n:as'
               return []
             else do
               put as'
               as'' <- takeUnder' n (acc'+1)
               return $ a:as''
    
generateIndices :: (MonadRandom m) =>
  Int -> Prob -> m [Int]
generateIndices n p = generateIndices' 0 n p where
  generateIndices' acc n p =
    do i <- geo p
       let acc' = i + acc
       case acc' >= n of
         True -> return []
         False -> do
           is <- generateIndices' (acc'+1) n p 
           return $ i:is

--should use mono-traversable and sequence
randomlyM :: (MonadRandom m) =>
  Prob ->
  Int ->
  (a -> m a) ->
  S.Seq a ->
  m (S.Seq a)
randomlyM p n f as = undefined

randomly :: (MonadRandom m, Functor m) =>
  Prob -> --probability of action
  Int ->  --size in each index 
  ([Int] -> a -> m a) -> --action
  S.Seq a -> --sequence to act on
  m (S.Seq a)
randomly p n action as = do
  indices <- generateIndices (n * S.length as) p
  onIndices n action indices as

onIndices :: (MonadRandom m, Functor m) => 
  Int ->
  ([Int] -> a -> m a) ->
  [Int] ->
  S.Seq a ->
  m (S.Seq a)
onIndices n f indices as = evalStateT (go n f as) indices where
  go n f as = do 
    emptyList <- gets null
    if emptyList
      then return as
      else do
        i <- skipping n
        let (top, bottom) = S.splitAt i as
        if S.null bottom
          then return top
          else do
            is <- takeUnder n
            a <- lift $ f is $ S.index bottom 0
            bottom' <- go n f $ S.drop 1 bottom
            return $ top S.>< S.singleton a S.>< bottom'

rmhc :: (MonadRandom m, Functor m) =>
  Int -> --Size of individuals
  Int -> --Number of generations
  Prob -> --Probability of mutation
  [Op a] -> --Operators
  (Ind32 -> m Double) -> --Evaluator
  m (Ind32, Double)
rmhc is gens mutRate ops eval = do
  let bits = bitsUsed ops
  initGenes <- ind32 is $ fromIntegral bits
  fitness <- eval initGenes
  let loop 0 ind = return ind
      loop n ind@(genes, fitness) = do
        genes' <- randomly mutRate bits mutateLocus genes
        fitness' <- eval genes'
        let ind' = (genes', fitness)
        loop (pred n) (maxBy snd ind' ind)
    in
      loop gens (initGenes, fitness)

smap :: (a -> b) -> S.Seq a -> S.Seq b
smap = fmap

rgepRun decoder a as = runProgramWithDefault a . F.toList . smap decoder $ as

testRMHC = do
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  (ind, fit) <- runRandIO $ rmhc 40 100000 0.1 ops (return . rgepRun decoder 0)
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

{- GA RGEP -}

crossPair (a, b) = do
  crossPoint <- fromRange (S.length a)
  let (top,  bottom)  = S.splitAt crossPoint a
  let (top', bottom') = S.splitAt crossPoint b
  return (top S.>< bottom', top' S.>< bottom)

unpair as = (fmap fst as, fmap snd as)

--this should probably shuffle the population to ensure random matches
crossover :: (MonadRandom m, Functor m) =>
  Prob -> Pop32 -> m Pop32
crossover p population = do
  let (top, bottom) = S.splitAt midPoint population
      midPoint = S.length population `div` 2
      pairs = S.zip top bottom
  crossedPairs <- randomly p 1 (const crossPair) pairs
  let (firstHalf, secondHalf) = unpair crossedPairs
  return $ firstHalf S.>< secondHalf

seqToVect :: S.Seq a -> V.Vector a
seqToVect = V.fromList . F.toList

vectToSeq :: V.Vector a -> S.Seq a
vectToSeq = S.fromList . F.toList

tournamentSelection :: (MonadRandom m) =>
  Int -> S.Seq (a, Double) -> m (S.Seq a)
tournamentSelection size population = do
  let vectPop = seqToVect population
      len = V.length vectPop
  indices <- replicateM len $ replicateM size (fromRange len)
  let choices = map (map (vectPop V.!)) indices 
      winners = fmap (maximumBy (compare `on` snd)) choices
  return . fmap fst . S.fromList $ winners

ensure (ind, ind') = (higher, lower) where
  higher = maxBy snd ind ind'
  lower = minBy snd ind ind'

choose p pair = let (a, b) = ensure pair in do
  choice <- sample stdUniform
  return $ if choice < p
    then a
    else b
stochasticTournament :: (MonadRandom m, Applicative m) =>
  Prob -> S.Seq (a, Double) -> m (S.Seq a)
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


mutateLocus :: (MonadRandom m) => [Int] -> Word32 -> m Word32
mutateLocus is n = return $! foldr flipBit n is where
  flipBit index n = n `xor` (1 `shiftL` index)

mutate bits indices ind = onIndices bits mutateLocus indices ind

pointMutation :: (MonadRandom m, Functor m) =>
  Prob -> Int -> Pop32 -> m Pop32
pointMutation pm bits genes = randomly pm totalBits (mutate bits) genes where
  totalBits = S.length genes * bits 

evaluation :: (Functor m, MonadRandom m) =>
  (a -> m Double) ->
  S.Seq a ->
  m (S.Seq (a, Double))
evaluation eval pop = S.zip pop <$> T.mapM eval pop

geneticAlgorithm :: (Functor m, MonadRandom m) =>
  Int -> -- Population size
  Int -> -- Individual size
  Int -> -- Generations
  Prob -> -- pm
  Prob -> -- pc
  (Ind32 -> m Double) -> -- Fitness Evaluation
  m Pop32
geneticAlgorithm ps is gens pm pc eval = do
  initialPopulation <- pop32 ps is 1
  let loop 0 pop = return pop
      loop gens pop = do
        popEvaled <- evaluation eval pop
        popSelected <- tournamentSelection 2 popEvaled
        popCrossed <- crossover pc popSelected
        popMutated <- pointMutation pm 1 popCrossed
        loop (pred gens) popMutated
    in
      loop gens initialPopulation

ones :: S.Seq Word32 -> Double
ones ind = fromIntegral $ F.sum ind

testGA = do
  let evaluate ind = return $ ones ind
  pop <- runRandIO $ geneticAlgorithm 10 20 1000 0.01 0.6 evaluate
  population <- runRandIO $ evaluation evaluate pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "fitness = %f\n" fit

{- Original RGEP -}
--rotation needs implementation
rotIndividual ind = do
  rotationPoint <- fromRange $ S.length ind
  let (top, bottom) = S.splitAt rotationPoint ind
  return $ bottom S.>< top
rotation :: (Functor m, MonadRandom m) =>
  Prob -> 
  Pop32 ->
  m Pop32
rotation pr pop = randomly pr 1 (const rotIndividual) pop

foldInHalf s = S.zip top bottom where
  (top, bottom) = S.splitAt midPoint s
  midPoint = S.length s `div` 2

exchange as bs = exchange' S.empty S.empty as bs

exchange' rest rest' [] [] = (rest, rest')
exchange' rest rest' (a:as) (b:bs) = exchange' (rest S.>< a) (rest' S.>< b) bs as

splits [] as = [as]
splits (p:points) as = top : splits points bottom where
  (top, bottom) = S.splitAt p as

multicrossPair n (a, b) = do
  let len = S.length a
  points <- sort <$> replicateM n (fromRange len)
  return $ exchange (splits points a) (splits points b)

multipointCrossover :: (Functor m, MonadRandom m) =>
  Prob -> -- probability of crossover
  Int -> --number of cross points
  Pop32 -> --population to crossover
  m Pop32
multipointCrossover pc points pop = do
  crossedPairs <- randomly pc 1 (const (multicrossPair points)) $ foldInHalf pop
  let (firstHalf, secondHalf) = unpair crossedPairs
  return $ firstHalf S.>< secondHalf

rgep :: (MonadRandom m, Applicative m, Functor m) =>
  Int -> --individual size
  Int -> --population size
  [Op a] -> --operators
  Prob -> -- pm
  Prob -> -- pr
  Prob -> -- pc1
  Prob -> -- pc2
  Prob -> -- tournament selection prob
  Int -> -- generations
  a -> -- default value
  (a -> m Double) ->
  m Pop32
rgep is ps ops pm pr pc1 pc2 pt gens def eval = do
  let bits = bitsUsed ops
      decoder = decode ops
  pop <- pop32 ps is bits
  let loop 0 pop = return pop
      loop gens pop = do
        popEvaled <- evaluation (eval . rgepRun decoder def) pop
        popSelected <- stochasticTournament pt popEvaled
        popCrossed1 <- crossover pc1 popSelected
        popCrossed2 <- multipointCrossover pc2 2 popCrossed1
        popMutated <- pointMutation pm 1 popCrossed2
        popRotated <- rotation pr popMutated
        loop (pred gens) popRotated
    in
      loop gens pop

testRGEP = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Double]
      decoder = decode ops
  pop <- runRandIO $ rgep 20 10 ops 0.01 0.1 0.6 0.6 0.75 100 0 return
  population <- runRandIO $ evaluation (return . rgepRun decoder 0) pop
  let (ind, fit) = F.maximumBy (compare `on` snd) population
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit

