{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
module SeqZip where


import Data.Monoid
import Data.List.Split
import qualified Data.Sequence as S

import Control.Applicative
import Control.Monad.State
import Control.Comonad

import Debug.Trace


data b :>: a = b :>: a  deriving (Show, Eq, Ord)

instance Functor ((:>:) b) where
  fmap f (b :>: a) = b :>: f a

data Top = Top deriving (Show)

--type families could give "split" type.
type Layer0 a = Top :>: a
type Layer1 a = S.Seq a :>: a
type Layer2 a = (S.Seq (S.Seq a)) :>: S.Seq a :>: a


combine as a as' = as S.>< (S.singleton a) S.>< as'

upOne ((c :>: (b, b')) :>: a) = c :>: combine b a b'

downOne (c :>: a) = (c :>: (S.empty, S.drop 1 a)) :>: (S.index a 0)

downOver i (c :>: a) = (c :>: (S.take i a, S.drop (i+1) a)) :>: (S.index a i)

zipRight 0 z = z

zipRight i z@((c :>: (as, as')) :>: a) =
  case i <= S.length as' of
    True ->
      (c :>: (combine as a (S.take (i-1) as'), S.drop i as')) :>: S.index as' (i-1)
    False ->
      zipRight (S.length as') z

remainingLeft  ((c :>: (as, as')) :>: a) = S.length as
remainingRight ((c :>: (as, as')) :>: a) = S.length as'

unzipper :: (Top :>: a) -> a
unzipper (_ :>: a) = a

getTip (c :>: tip) = tip
getsTip f (c :>: a) = f a

zipper as = Top :>: as

modifyWith :: (b :>: a) -> (a -> a) -> (b :>: a)
modifyWith (as :>: a) f = as :>: (f a)

modifyWithM :: (Monad m) => (b :>: a) -> (a -> m a) -> m (b :>: a)
modifyWithM (as :>: a) f = do
  a' <- f a
  return $ as :>: a'

skipOne = zipRight 1


