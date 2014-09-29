{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, TemplateHaskell, FlexibleContexts #-}
module Types where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Word
import Data.Random
import Data.Monoid
import Data.Conduit
import Data.Function
import Data.Random.Source
import Data.Random.Internal.Words

import Math.Probable.Random

import Control.Monad.Primitive

import Common

type Prob = Double

{- RGEP Types -}
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

type Decoder a = Word32 -> Op a

{- General Types -}

type Ind a = S.Seq a
type Pop a = S.Seq a

type IndV a = V.Vector a
type PopV a = V.Vector a

type Ind32 = Ind Word32
type Pop32 = Pop Ind32

type IndV32 = IndV Word32
type PopV32 = PopV IndV32

type IndR = S.Seq Double
type PopR = S.Seq IndR

type IndVR = V.Vector Double
type PopVR = V.Vector IndVR

type IndBits = Int
type PopBits = S.Seq IndBits

type IndVBits = Int
type PopVBits = V.Vector IndBits

{- Algorithm Types -}
data RGEP d p = RGEP
data GA r d l p = GA

{- Problem Types -}
data BitSet = BitSet
data LargeNumber = LargeNumber
data ANN = ANN

{- Monad Random Instances -}
$(monadRandom [d|
  instance (PrimMonad m) => MonadRandom (RandT m) where
    getRandomWord8 = word8
    getRandomWord16 = word16
    getRandomWord32 = word32
    getRandomWord64 = word64
    getRandomDouble = double
  |])

{- Additional Genericness -}
type family HasRepr a 
type instance HasRepr (GA r d l p) = r
type instance HasRepr (RGEP d p) = Pop32

type family HasSolution a
type instance HasSolution (RGEP d p) = p
type instance HasSolution (GA r d l p) = p

type family HasLocus a
type instance HasLocus (RGEP d p) = Word32
type instance HasLocus (GA r d l p) = l

type family HasProblem a
type instance HasProblem (RGEP d p) = p
type instance HasProblem (GA r d l p) = p

