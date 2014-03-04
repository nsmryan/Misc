module Types where

import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word

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

type Decoder a = Word32 -> Op a

type Ind = S.Seq
type Pop a = S.Seq a

type Ind32 = Ind Word32
type Pop32 = Pop Ind32

type IndR = S.Seq Double
type PopR = S.Seq IndR

type IndBits = Int
type PopBits = S.Seq IndBits

