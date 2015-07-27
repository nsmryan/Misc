{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  GADTs #-}
{-# LANGUAGE  DeriveFunctor #-}
module RGEP where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Random
import Data.Tree

import Math.Polynomial

import Pipes
import qualified Pipes.Safe as PS

import Diagrams.TwoD.Layout.Tree
import Diagrams.Prelude as D hiding (rotation)

import qualified Pipes.Prelude as P

import Control.Monad.Operational as Op


import Common hiding ((<>))
import Types
import UtilsRandom
import PointMutation
import Crossover
import PipeOperators
import Rotation
import Selection
import Evaluation
import Utils


{- RGEP Types -}

data StackI a b where
  Pop :: StackI a a
  Push :: a -> StackI a ()
  Try :: StackAST a () -> StackI a ()

type StackAST a b = Program (StackI a) b

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

--TODO could have a combining applicative/monoid/something instance
data Op a = Op { name :: Sym
               , arity :: Arity
               , program :: StackAST a ()
               , treeProgram :: StackAST (Tree Sym) ()
               }

instance Show (Op a) where
  show = name

instance Eq (Op a) where
  (Op sym arity _ _) == (Op sym' arity' _ _) = sym == sym' && arity == arity'


type Decoder a = Word32 -> Op a

data RGEPPhenotype a = RGEPPhenotype
                     { rgepTree :: Tree Sym
                     , rgepTreeless :: a
                     }
                     deriving (Show, Eq)

type RGEPExpressed a = Expressed Ind32 (RGEPPhenotype a)

type RGEPEval a = Evaled Ind32 (RGEPPhenotype a)


pop :: StackAST a a
pop = singleton Pop

push :: a -> StackAST a ()
push a = singleton $ Push a

try :: StackAST a () -> StackAST a ()
try = singleton . Try

evalStack :: StackAST a b -> (Maybe b, [a])
evalStack prog = evalStack' prog []

evalStack' :: StackAST a b -> [a] -> (Maybe b, [a])
evalStack' prog as = eval (Op.view prog) as
  where
    eval :: ProgramView (StackI a) b -> [a] -> (Maybe b, [a])
    eval (Return b)      as     = (Just b, as)
    eval (Pop :>>= k)    []     = (Nothing, [])
    eval (Pop :>>= k)    (a:as) = evalStack' (k a) as
    eval (Push a :>>= k) as     = evalStack' (k ()) (a:as)
    eval (Try p :>>= k)  as     = case evalStack' p as of
                                    (Nothing, _) -> evalStack' (k ()) as
                                    (Just b, as') -> evalStack' (k ()) as'

showPrefix (Node nam []) = " " ++ nam
showPrefix (Node nam as) = " (" ++ nam ++ (concatMap showPrefix as) ++ ")"
showPostfix (Node nam []) = " " ++ nam
showPostfix (Node nam as) = concatMap showPostfix as ++ " " ++ nam

treeDiagram tree =
  renderTree ((<> circle 1 # fc white) . centerXY . text)
             (\p p' -> (p ~~ p'))
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tree)
  # alignL # pad 1.1 # scale 20

{- Operators -}
mkOp1 name f = Op name (Arity 1 1) onHead buildTree where
  onHead = try (pop >>= push . f)
  buildTree = try (pop >>= \tree -> push (Node name [tree]))

mkOp2 name f = Op name (Arity 2 1) onTwo buildTree where
  onTwo = try $ do
    a <- pop
    b <- pop
    push (f a b)
  buildTree = try $ do
    a <- pop
    b <- pop
    push $ Node name [a, b]

mkTerm name v =
  Op name (Arity 0 1) (push v) (push $ Node name [])

--Generic operators
idOp = mkOp1 "id" id

-- Plain arithmatic operations
plusOp, minusOp, timesOp, divOp, incOp, zeroTerm, oneTerm, twoTerm :: Op Double
plusOp = mkOp2 "+"    (+)
minusOp = mkOp2 "-"   (-)
timesOp = mkOp2 "*"   (*)
safeDivOp = mkOp2 "/" (safeDiv)
divOp = mkOp2 "/"     (/)
incOp = mkOp1 "inc"   (1.0 +)
zeroTerm = mkTerm "0" (0.0)
oneTerm = mkTerm "1"  (1.0)
twoTerm = mkTerm "2"  (2.0)

safeDiv x y = if y == 0 then 0 else x / y

arithOps = [plusOp, minusOp, timesOp, safeDivOp, incOp, zeroTerm, oneTerm, twoTerm]

noop = try $ do
  a <- pop
  push a

dupProg = try $ do
  a <- pop
  push a
  push a

-- Stack operations
dup    = Op "dup"  (Arity 1 2) dupProg
dropOp = Op "drop" (Arity 1 0) $ try pop
swap   = Op "swap" (Arity 2 2) $ try $ do
  a <- pop
  b <- pop
  push a
  push b
tuck   = Op "tuck" (Arity 2 3) $ try $ do
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
allBitsTerm size = mkTerm "All" ((2 ^ (round $ logBase 2 $ fromIntegral size)) - 1)
noBitsTerm size  = mkTerm "0" 0

keepOnly n val = ((2 ^ n) - 1) .&. val

setBitOps n = [setBitOp i (fromIntegral n) | i <- [0..n-1]]
setBitTerms n = [setBitTerm i (fromIntegral n) | i <- [0..n-1]]

--Set operators extended with extra operators to set each bit.
bitSetOpsExtended n = setBitOps n ++ bitSetOps n
--Operations for finding a set of items.
bitSetOps n =
  bitSetOpsSimple n
  ++
  map ($ (fromIntegral n)) [complementOp, allBitsTerm, noBitsTerm]
--Simple set of  operations for finding a set. Removes advantage of setting and clearing all bits.
bitSetOpsSimple n =
  bitSetOps n
  ++
  map ($(fromIntegral n)) [unionOp, removeOp, intersectionOp]

--Polynominal (single variable)
polyOp2 nam op = mkOp2 nam op --(\f g x -> f x `op` g x)
polyConst nam v = mkTerm nam $ constPoly v

polyPlus = polyOp2 "+" addPoly
polyMinus = polyOp2 "-" (\p p' -> p `addPoly` (negatePoly p'))
polyTimes = polyOp2 "*" (multPoly)
polyX = mkTerm "x" x
polyZero = polyConst "0"  0.0
polyOne  = polyConst "1"  1.0
polyTwo  = polyConst "2"  2.0
polyFive = polyConst "5"  5.0
polyTen  = polyConst "10" 10.0

polyOps :: [Op (Poly Double)]
polyOps = [polyPlus, polyMinus, polyTimes, polyX,
           polyZero, polyOne,   polyTwo,   polyFive, polyTen]

errorOn :: [(Double, Double)] -> (Double -> Double) -> Double
errorOn trainingData f =
  abs . sum $ map (\(input, result) -> (f input - result) ^ 2) trainingData

--Extra function operations
logOp = mkOp1 "log" (log)


-- Function creation
-- Decision trees
-- Neural Networks
--Tree creating ops

{- Gene Expression -}
runRGEPProgram defaul opList =
  let result = evalStack $ foldr (>>) noop opList
  in case result of
       (_, [])     -> defaul
       (Just _, a:as)  -> a

rgepRun :: Decoder a -> a -> Ind32 -> RGEPExpressed a
rgepRun decoder defaul as =
  let decoded = map decoder $ F.toList as
      opList = map program decoded
      treeless = runRGEPProgram defaul opList
      tree = runRGEPProgram (Node "" []) $ map treeProgram decoded
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
        popCrossed1 <- singlePointCrossoverM is ps pc1 popSelected
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


