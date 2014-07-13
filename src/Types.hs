module Types where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Random
import Data.Monoid
import Data.Conduit

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

