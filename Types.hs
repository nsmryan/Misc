module Types where

import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Random
import Data.Conduit

import System.Log.Logger

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

type Ind32 = Ind Word32
type Pop32 = Pop Ind32

type IndR = S.Seq Double
type PopR = S.Seq IndR

type IndBits = Int
type PopBits = S.Seq IndBits

{- Application Control Types -}
--TODO return Either String d in conduits
--MonitorResult is a bit lazy. this should be data
--that is used by a downstream monitor.
data Result d = MonitorResult (IO ())
              | DataResult d
              | LogResult Priority String

--type Operator d = Reader Config (ConduitM () (Result d) IO d)
--IO could be MonadRandom m and allow other intepreters.
type Heal m d = ConduitM () (Result d) m d
type HealIO d = Heal IO d

