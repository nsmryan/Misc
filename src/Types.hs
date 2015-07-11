{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import Data.Configurator as C
import Data.Configurator.Types
import Data.Word
import Data.Random
import Data.Monoid
import Data.Function
import Data.Random
import Data.Random.Source
import Pipes.Safe

--import Math.Probable.Random
import System.Random.MWC

import Pipes

import Control.Monad.Primitive
import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Control.Exception as E
import Control.Monad.Operational

import qualified GHC.Generics as G

import Common

{-
consider having several built-in fitness functions for each algorithm like
data GAProblems = BinaryClassifier | Ones | RoyalRoad
and a way of specifying specific algorithms
data Algorithms = RGEP | GA | UGA | PSO

requiring a "dataSet" file in the config,
and then having a version that is calleable from the command line to perform these.
-}

{- Probability -}
type Prob = Double


{- Heal Monad -}
--newtype Heal a = Heal { runHeal :: Rand a }

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
               }

data ExprTree a = ExprTree
                { treeRoot :: Op a
                , treeChildren :: [ExprTree a]
                }

instance Show (Op a) where
  show = name

type Decoder a = Word32 -> Op a

type Expresser a = (S.Seq (Op a)) -> a

data RGEPPhenotype a = RGEPPhenotype
                     { rgepTree :: ExprTree a
                     , rgepTreeless :: a
                     }

type RGEPExpressed a = Expressed Ind32 (RGEPPhenotype a)

type RGEPEval a = Evaled Ind32 (RGEPPhenotype a)


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

newtype Genetic a = Genetic { unGenes :: a }

data Expressed a b = Expressed
                   { genetic  :: a
                   , expression :: b
                   } deriving (Eq, Show, G.Generic)

data Evaled a b = Evaled
                { expressed :: Expressed a b
                , fitness :: Double
                } deriving (Show, Eq, G.Generic)

instance (NFData a, NFData b) => NFData (Expressed a b) where rnf = genericRnf
instance (NFData a, NFData b) => NFData (Evaled a b) where rnf = genericRnf

{- Pipe Helpers -}
data Chunk f a = Piece (f a)
               | Location a
               | Flush deriving (Show, Eq)


data Choose a = Chosen a | NotChosen a

--TODO consider homogenous pairs with foldable to generalize
data Tournament a = Tournament a a

{- Application Blocks -}
type Block a b = ReaderT Config IO (Pipe a b (RVarT (SafeT IO)) ())

{- Algorithm Types -}
data RGEP d p = RGEP
data GA r d l p = GA

{- Problem Types -}
data BitSet = BitSet
data LargeNumber = LargeNumber
data ANN = ANN


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

