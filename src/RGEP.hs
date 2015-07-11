{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  GADTs #-}
{-# LANGUAGE  DeriveFunctor #-}
module RGEP where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.List.Split
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Random
--import Data.Conduit

import qualified Pipes.Prelude as P

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Free
import Control.Monad.Operational


import Common
import Types
import UtilsRandom
import PointMutation
import Crossover
import Rotation
import Selection
import Evaluation
import Utils



pop :: StackAST a a
pop = singleton Pop

push :: a -> StackAST a ()
push a = singleton $ Push a

try :: StackAST a () -> StackAST a ()
try = singleton . Try

evalStack :: StackAST a b -> (Maybe b, [a])
evalStack prog = evalStack' prog []

evalStack' :: StackAST a b -> [a] -> (Maybe b, [a])
evalStack' prog as = eval (view prog) as
  where
    eval :: ProgramView (StackI a) b -> [a] -> (Maybe b, [a])
    eval (Return b)      as     = (Just b, as)
    eval (Pop :>>= k)    []     = (Nothing, [])
    eval (Pop :>>= k)    (a:as) = evalStack' (k a) as
    eval (Push a :>>= k) as     = evalStack' (k ()) (a:as)
    eval (Try p :>>= k)  as     = case evalStack' p as of
                                    (Nothing, _) -> evalStack' (k ()) as
                                    (Just b, as') -> evalStack' (k ()) as'

{- Operators -}
mkOp1 name f = Op name (Arity 1 1) onHead where
  onHead = pop >>= push . f

mkOp2 name f = Op name (Arity 2 1) onTwo where
  onTwo = do
    a <- pop
    b <- pop
    push (f a b)

mkTerm name v = Op name (Arity 0 1) (push v)

--Generic operators
idOp = mkOp1 "id" id

-- Plain arithmatic operations
plusOp, minusOp, timesOp, divOp, incOp, zeroTerm, oneTerm, twoTerm :: Op Double
plusOp = mkOp2 "+"    (+)
minusOp = mkOp2 "-"   (-)
timesOp = mkOp2 "*"   (*)
divOp = mkOp2 "/"     (/)
incOp = mkOp1 "inc"   (1.0 +)
zeroTerm = mkTerm "0" (0.0)
oneTerm = mkTerm "1"  (1.0)
twoTerm = mkTerm "2"  (2.0)

arithOps = [plusOp, minusOp, timesOp, divOp, incOp, zeroTerm, oneTerm, twoTerm]

noop = do
  a <- pop
  push a

dupProg = do
  a <- pop
  push a
  push a

-- Stack operations
dup    = Op "dup"  (Arity 1 2) dupProg
dropOp = Op "drop" (Arity 1 0) pop
swap   = Op "swap" (Arity 2 2) $ do
  a <- pop
  b <- pop
  push a
  push b
tuck   = Op "tuck" (Arity 2 3) $ do
  a <- pop
  b <- pop
  push a
  push b
  push a


stackOps = [dup, dropOp, swap, tuck]

-- Bit Set operators
unionOp, removeOp, intersectionOp, complementOp :: Int -> Op Integer
unionOp size        = mkOp2 "U" (.|.)
removeOp size       = mkOp2 "//" $ \a b -> keepOnly size (a .&. (complement b))
intersectionOp size = mkOp2 "I" (.&.)
complementOp size   = mkOp1 "Not" (keepOnly size . complement)

setBitOp, clearBitOp, setBitTerm :: Int -> Int -> Op Integer
setBitOp bitNum size   = mkOp1 ("Set " ++ show bitNum) (flip setBit bitNum)
clearBitOp bitNum size = mkOp1 ("Clear " ++ show bitNum) (flip clearBit bitNum)
setBitTerm bitNum size = mkTerm (show bitNum) (bit bitNum)

allBitsTerm, noBitsTerm :: Int -> Op Integer
allBitsTerm size = mkTerm "All" ((2 ^ size) - 1)
noBitsTerm size  = mkTerm "0" 0

keepOnly n val = ((2 ^ n) - 1) .&. val

--Set operators extended with extra operators to set each bit.
bitSetOpsExtended n = bitSetOps n ++ [setBitOp i (fromIntegral n) | i <- [0..n-1]]
--Operations for finding a set of items.
bitSetOps n = bitSetOpsSimple n ++ map ($ (fromIntegral n)) [complementOp, allBitsTerm, noBitsTerm]
--Simple set of  operations for finding a set. Removes advantage of setting and clearing all bits.
bitSetOpsSimple n =
  [setBitTerm i (fromIntegral n) | i <- [0..n-1]]
  ++
  map ($(fromIntegral n)) [unionOp, removeOp, intersectionOp]

--Polynominal (single variable)
polyOp2 nam op = mkOp2 nam (\f g x -> f x `op` g x)
polyConst nam v = mkTerm nam $ const v

polyPlus = polyOp2 "+" (+)
polyMinus = polyOp2 "-" (-)
polyTimes = polyOp2 "*" (*)
polyX = mkOp1 "x" id
polyZero = polyConst "0"  0.0
polyOne  = polyConst "1"  1.0
polyTwo  = polyConst "2"  2.0
polyFive = polyConst "5"  5.0
polyTen  = polyConst "10" 10.0

polyOps :: [Op (Double -> Double)]
polyOps = [polyPlus, polyMinus, polyTimes, polyX, polyZero, polyOne, polyTwo, polyFive, polyTen]

errorOn :: [(Double, Double)] -> (Double -> Double) -> Double
errorOn trainingData f =
  negate . sum . map (^2) $ zipWith (-) (map snd trainingData)
                               (map (f . fst) trainingData)

--Extra function operations
logOp = mkOp1 "log" (log)


-- Function creation
-- Decision trees
-- Neural Networks
--Tree creating ops
--mkTree op = mkOp1 treeRoot
--mkChildren = treeChildren :: [ExprTree a]

{- Gene Expression -}
rgepRun :: Decoder a -> a -> Ind32 -> RGEPExpressed a
rgepRun decoder defaul as =
  let opList = map (program . decoder) $ F.toList as
      result = evalStack $ foldr (>>) noop opList
      treeless = case result of
                   (_, [])     -> defaul
                   (Just _, a:as)  -> a
      --TODO create ops from existing ones which build trees.
      tree = ExprTree undefined undefined
  in
    Expressed as $ RGEPPhenotype tree treeless

{- Evaluation -}

rgepEvalInd :: (a -> RVarT m Double) -> RGEPExpressed a -> RVarT m Double
rgepEvalInd eval individual = eval . rgepTreeless . expression $ individual

rgepEvaluation ::
  (a -> RVarT m Double) ->
  Decoder a ->
  a ->
  Pop32 ->
  RVarT m (Pop (RGEPEval a))
rgepEvaluation eval decoder def pop = do
  let decoded = rgepRun decoder def <$> pop
  fitnesses <- T.sequence $ smap (rgepEvalInd eval) decoded
  return $ S.zipWith Evaled decoded fitnesses

{- Express raw bits -}

bitsUsed ops = let (terms, nonterms) = splitSymbols ops in
  if (not $ null terms) && (not $ null nonterms)
    then max (bitsRequired terms) (bitsRequired nonterms)
    else error "There must be at least one terminal symbol and one nonterminal symbol"

bitsRequired ops = 1 + (ceiling $ logBase 2 (fromIntegral $ length ops))

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


{- Elitism -}
elitism ::
  Int ->
  (Pop (Evaled a b) -> RVarT m (Pop a)) ->
  Pop (Evaled a b) ->
  RVarT m (Pop a)
elitism k select pop = do
  let (elite, common) = S.splitAt k $ S.sortBy compareFitnesses pop
  selected <- select common
  return (genetics elite S.>< selected)


{- Basic RGEP implementation -}
rgep ::
  (Monad m) =>
  Int    -> --population size
  Int    -> --individual size
  [Op a] -> --operators
  Prob   -> -- pm
  Prob   -> -- pr
  Prob   -> -- pc1
  Prob   -> -- pc2
  Prob   -> -- tournament selection prob
  Int    -> -- generations
  a      -> -- default value
  (a     -> RVarT m Double)              -> --fitness function
  RVarT m (Pop (RGEPEval a))
rgep ps is ops pm pr pc1 pc2 pt gens def eval = do
  let bits = bitsUsed ops
      decoder = decode ops
      evaluator pop = rgepEvaluation eval decoder def pop
  pop <- pop32 ps is bits
  let loop 0 pop = return pop
      loop gens pop = do
        popEvaled <- evaluator pop
        popSelected <- elitism 1 (stochasticTournament pt) popEvaled
        popCrossed1 <- singlePointCrossoverM pc1 popSelected
        popCrossed2 <- multipointCrossoverM pc2 2 popCrossed1
        popMutated <- pointMutation pm is 1 popCrossed2
        popRotated <- rotation pr popMutated
        loop (pred gens) popRotated
    in do
          finalPopulation <- loop gens pop
          evaluator finalPopulation

rgepExpressionBlock :: [Op a] -> a -> Block Pop32 (Pop (RGEPExpressed a))
rgepExpressionBlock ops defaul = let decoder = decode ops in
  return $ P.map (fmap $ rgepRun decoder defaul)


