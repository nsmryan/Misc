module SparseSeq where


import Data.Monoid
import qualified Data.Sequence as S

import Control.Applicative

data Movement a = Left Int
                | Right Int
                | MoveDown
                | ApplyOn (a -> a)

data Trav s z e =
  Trav
    {
      toTrav :: s -> z
    , fromTrav :: z -> s
    , extractElement :: z -> e
    , moveLeftTrav :: Int -> z -> z
    , moveRightTrav :: Int -> z -> z
    , applyOnTrav :: (e -> e) -> z -> z
    }

data SeqZip a = SeqZip { behind :: S.Seq a, focus :: a, ahead :: S.Seq a }

seqtoZip seq = SeqZip (seqTail seq) (seqHead seq) S.empty

seqFromZip (SeqZip left center right) = left S.>< (center S.<| right)

seqHead s = S.index s 0
seqTail s = S.drop 1 s

moveLeft 0 s = s
moveLeft n (SeqZip b c a) = SeqZip b' c' a' where
  (grabbed, left) = S.splitAt (n-1) a'
  b' = b S.>< (c S.<| (seqTail grabbed))
  c' = seqHead grabbed
  a' = left

moveRight 0 s = s
moveRight n (SeqZip b c a) = SeqZip b' c' a' where
  (grabbed, left) = S.splitAt (n-1) b'
  b' = left
  c' = seqHead grabbed
  a' = b S.>< (c S.<| (seqTail grabbed))

applyOn f (SeqZip b c a) = SeqZip b (f c) a

seqTrav :: Trav (S.Seq a) (SeqZip a) a
seqTrav = Trav seqtoZip seqFromZip focus moveLeft moveRight applyOn

movements :: [Int] -> [(Int, a -> a)] -> [Movement a]
movements shape indices = undefined


--The type of sparse sequences. This type holds its size,
--as it cannot be determined from its elements as in a normal sequence
--The sequence of elements contains the number of unused places between
--consequtive elements.
data SparseSeq a = SparseSeq { seqSize :: Int, seq :: S.Seq (Int, a) } deriving (Show)

fromSeq seq = SparseSeq len (S.zip (S.fromList (replicate len 0)) seq) where len = S.length seq
toSeq filler (SparseSeq size elements) = elements `mergeSparse` (S.replicate size filler)
sparseFromList = fromSeq . S.fromList
--sparseToList filler = S.fromList . toSeq filler

sparseSingleton a = SparseSeq 1 (S.singleton a)

offsetElements offset elements = ((\ (ix, a) -> (ix + offset, a)) <$> elements)

instance Monoid (SparseSeq a) where
  mempty = SparseSeq 0 S.empty
  (SparseSeq size elements) `mappend` (SparseSeq size' elements') = 
    SparseSeq resultSize resultElements where
      resultSize = size + size'
      resultElements = elements S.>< (offsetElements size elements')

breakAt seq ix = let (front, back) = S.splitAt ix seq in (front, seqHead back, seqTail back)
sparseApply :: SparseSeq (a -> a) -> S.Seq a -> S.Seq a
sparseApply (SparseSeq size fs) as = sparseApply' fs as

sparseApply' fs as | S.null fs = as
sparseApply' fs as =
  let
    (ix, f) = seqHead fs 
    (front, a, back) = as `breakAt` ix
  in
    front S.>< S.singleton (f a) S.>< sparseApply' (seqTail fs) back


--requires knowing distance from last element to end of sequence.
mergeSparseWith :: (a -> a -> a) -> S.Seq (Int, a) -> S.Seq (Int, a) -> S.Seq (Int, a)
mergeSparseWith f seq seq' = undefined
mergeSparse seq seq' = mergeSparseWith const


n `fitIn` m = (m `div` n) + if m `mod` n == 0 then 0 else 1
deepen :: Int -> SparseSeq a -> SparseSeq (SparseSeq a)
deepen n (SparseSeq size as) = SparseSeq (n `fitIn` size) as' where
  as' = 

--applyOver :: (SparseSeq (a -> b)) -> S.Seq a -> S.Seq b
--applyOver sparse dense = mergeSparseWith ($) sparse (fromSeq dense)


