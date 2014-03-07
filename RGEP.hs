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
import Data.Conduit
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Text.Printf

import Debug.Trace

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Primitive
import Control.Monad.Primitive.Class
import Control.Monad.Trans.State.Lazy

import Types
import UtilsRandom
import PointMutation
import Crossover
import Rotation
import Selection
import PBIL
import RMHC
import Evaluation


--split pure and impure aspects of operators
--add conduit aspect
--system monitoring conduits
--add logging conduits
--add config file on top of conduits
d a = traceShow a a 


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
-- Function creation
-- Decision trees
-- Neural Networks

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

rgepRun decoder a as = runProgramWithDefault a . F.toList . smap decoder $ as

{- Express raw bits -}

bitsUsed ops = let (terms, nonterms) = splitSymbols ops in
  if length terms > 0 && length nonterms > 0
    then 1 + max (bitsRequired (length terms)) (bitsRequired (length nonterms))
    else error "There must be at least one terminal symbol and one nonterminal symbol"

bitsRequired n = ceiling $ logBase 2 (fromIntegral n)

splitSymbols ops = (filter ((==0) . inputs . arity) ops, filter ((/=0) . inputs . arity) ops)

decode :: [Op a] -> Decoder a
decode ops = uncurry decodeSymbols $ splitSymbols ops

decodeSymbols :: [Op a] -> [Op a] -> Decoder a
decodeSymbols terms nonterms = let
  termsV = V.fromList terms
  nontermsV = V.fromList nonterms
  in \ w -> let index = fromIntegral $ w `shiftR` 1
                symV = if testBit w 0 then nontermsV else termsV
                in symV V.! (index `mod` V.length symV)


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


{- RGEP RMHC -}
--TODO clean this up so it is a separate algorithm
--combining RMHC and RGEP
smap :: (a -> b) -> S.Seq a -> S.Seq b
smap = fmap

testRMHC = do
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
      decoder = decode ops
      bits = bitsUsed ops
  (ind, fit) <- runRandIO $ rmhc 40 bits 100000 0.1 (return . rgepRun decoder 0)
  printf "ind = %s\n" (show ind)
  printf "program = %s\n" $ show $ cleanProg . F.toList . smap decoder $ ind
  printf "fitness = %f\n" fit


{- Genetic Algorithms -}
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
        popMutated <- pointMutation pm is 1 popCrossed
        loop (pred gens) popMutated
    in
      loop gens initialPopulation


{- Elitism -}
elitism :: (MonadRandom m) =>
  Int ->
  (Pop (a, Double) -> m (Pop a)) -> 
  Pop (a, Double) ->
  m (Pop a)
elitism k select pop = do
  let (elite, common) = S.splitAt k $ S.sortBy (compare `on` snd) pop
  selected <- select common
  return (fmap fst elite S.>< selected)

elitismConduit ::
  Int ->
  (Pop (a, Double) -> HealIO (Pop a)) ->
  Pop (a, Double) ->
  HealIO (Pop a)
elitismConduit k select pop = do
  let (elite, common) = S.splitAt k $ S.sortBy (compare `on` snd) pop
  selected <- select common
  return (fmap fst elite S.>< selected)

{- RGEP -}
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
        popSelected <- elitism 1 (stochasticTournament pt) popEvaled
        popCrossed1 <- crossover pc1 popSelected
        popCrossed2 <- multipointCrossover pc2 2 popCrossed1
        popMutated <- pointMutation pm is 1 popCrossed2
        popRotated <- rotation pr popMutated
        loop (pred gens) popRotated
    in
      loop gens pop

{- RGEP Conduit -}
rgepConduit ::
  Int -> --individual size
  Int -> --population size
  [Op a] -> --operators
  Prob -> -- pm
  Prob -> -- pr
  Prob -> -- pc1
  Prob -> -- pc2
  Prob -> -- tournament selection prob
  a -> -- default value
  (a -> IO Double) ->
  ConduitM () (Result Pop32) IO ()
rgepConduit is ps ops pm pr pc1 pc2 pt def eval = do
  let bits = bitsUsed ops
      decoder = decode ops
      loop pop = do
        pop' <- evaluationConduit (eval . rgepRun decoder def) pop >>=
                elitismConduit 1 (stochasticTournamentConduit pt) >>=
                crossoverConduit pc1 >>=
                multipointCrossoverConduit pc2 2 >>=
                pointMutationConduit pm is 1 >>=
                rotationConduit pr
        yield $ DataResult pop'
        loop pop'
  pop <- liftIO $ pop32 ps is bits
  loop pop

