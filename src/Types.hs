{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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
--import Data.Conduit
import Data.Function

import Math.Probable.Random

import Pipes

import Control.Monad.Primitive
import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import qualified GHC.Generics as G

import Common


{- Probability -}
type Prob = Double

newtype R a = R { runR :: (RandT IO a) }

instance Monad R where
  return = R . return
  (R ma) >>= f = R $ do
    a <- ma
    runR (f a)

instance MonadIO R where
  liftIO action = R . RandT . const $ action

instance Functor R where
  fmap f (R ma) = R $ fmap f ma

instance Applicative R where
  pure = R . return
  rf <*> ra = do
    f <- rf
    a <- ra
    return $ f a

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

data ExprTree a = ExprTree
                { treeRoot :: Op a
                , treeChildren :: [ExprTree a]
                }

instance Show (Op a) where
  show = name

type Decoder a = Word32 -> Op a

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

{- Application Blocks -}
--type Block a b = Pipe a b (ReaderT Config R) ()
--type Block a b = ReaderT Config IO (Pipe a b R ())
type Block a b = ReaderT Config IO (Pipe a b R ())

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

