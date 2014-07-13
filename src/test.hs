{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
module Main where


import Data.Monoid
import Data.List.Split
import qualified Data.Sequence as S

import Control.Applicative
import Control.Monad.State
import Control.Comonad

import Debug.Trace


--TODO think aboue using RWS Int [Int] (Seq a)
--reading size, taking from indices, and writting used up sequence


data b :>: a = b :>: a  deriving (Show, Eq, Ord)

instance Functor ((:>:) b) where
  fmap f (b :>: a) = b :>: f a

data Top = Top deriving (Show)

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


skipOver n = do
  is <- get
  case is of
    [] -> return 0
    (i:is) -> return $ i `div` n

skipping :: Int -> State [Int] Int
skipping n = do
  (i:is) <- get
  if i == 0
    then do
      return 0
    else do 
      let (skips, m) = divMod i n
      --if (m /= 0)
      --  then put $ m : is
      --  else put is
      put $ m : is
      return $! skips

putBack i = modify (i:)

open = downOne . zipper
close = unzipper . upOne

--t s n = trace (s ++ show n) n
t s n = n

applyMOn ::
  Int ->
  (a -> State [Int] a) ->
  S.Seq a ->
  State [Int] (S.Seq a)
applyMOn n f as =
  close <$> (go (open (t "as = " as))) where
    go as = do
      is <- get
      if null (t ("n = " ++ show n ++ ", is: ") is) || (1 + remainingRight as == 0)
        then return as
        else do
          i <- skipping n
          let remaining = remainingRight as
          as' <- modifyWithM (zipRight (t "applying right by: " i) as) f
          case compare i remaining of
            LT -> go $ skipOne as'
            EQ -> return as'
            GT -> do
              putBack $! t "putting back " (i - remaining - 1)
              return $! as

-- each location, with given location size.
applyOn n f is as = evalState (applyMOn n f' as) is where
  f' a = do
    modify tail
    return $ f a

-- each location, with given location size.
applyOnEach = applyOn

-- 1 layer down, locus size = 1
applyOnLocus n f is as = evalState (applyMOn n f' as) is where
  f' as' = applyMOn 1 f'' as'
  f'' a = do
    modify tail
    return $ f a

-- 1 layer down, with given locus size
applyWithinLocus n m f is as = evalState (applyMOn (n*m) f' as) is where
  f' as' = applyMOn m f'' as'
  f'' as'' = do
    i <- gets head
    puts tail
    return (f i)

-- apply function taking index from list
applyIxOn n f is as = evalState (applyMIxOn n f as) is where

-- monadic appyly with function taking index
applyMIxOn n f as = applyMOn n f' as where
  f' a = do
    i <- gets head
    puts tail
    return (f i a)

puts f = do
  s <- get
  put (f s)




