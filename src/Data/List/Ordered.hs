{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TupleSections
  , TemplateHaskell
  , TypeFamilies
  , EmptyDataDecls
  , GADTs
  #-}
{- | Ordered list type with efficient and streaming observation in two directions.  -}
module Data.List.Ordered
( 
-- * Ordered list type.
  List
, Direction (..)

-- * Constructing ordered lists.
, empty
, singleton
, add
, merge
, merges

-- * Operations on ordered lists.
, null
, length
, mapMonotonic
, mergeMap
, filter
, take
, drop
, boundedLength
, nub
, sort

-- * Creation from Haskell sequences.
, fromSeq
, fromAscSeq
, fromDescSeq
, fromAscOrDescSeq
, fromSeqs

-- * Creation from Haskell lists.
, fromList
, fromAscList
, fromDescList
, fromAscOrDescList
, fromLists

-- * Creation from Maps.
, fromMap
, fromMapRange

-- * Observing as Haskell sequences.
, toSeq
, toUnorderedSeq
, toAscSeq
, toDescSeq

-- * Observing as regular Haskell lists.
, toList
, toUnorderedList
, toAscList
, toDescList

-- * List monad transformer wrapper.
, ListT (..)

-- * Internally used helper functions.
, mergeBy
, localNubBy
, mapRange
, smartLength
)
where

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Trans
import Data.Foldable (Foldable (foldMap), foldr)
import Data.Traversable (mapM)
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Sequence (Seq, breakl, viewl, ViewL(..), (<|))
import Generics.Regular hiding (from, to)
import Prelude hiding (filter, drop, take, length, null, mapM, foldr)
import qualified Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as S

-------------------------------------------------------------------------------

data List a  where
  From     ::          Direction -> Seq a  -> List a
  FromBoth ::          Seq a -> Seq a      -> List a
  Merge    ::          List a -> List a    -> List a
  Take     ::          Int -> List a       -> List a
  Drop     ::          Int -> List a       -> List a
  Sort     :: Ord a => Direction -> List a -> List a
  Nub      :: Eq a  => List a              -> List a

instance NFData a => NFData (Seq a) where
  rnf = rnf . F.toList

instance NFData a => NFData (List a) where
  rnf (From      d xs) = rnf (d,  xs)
  rnf (FromBoth xs ys) = rnf (xs, ys)
  rnf (Merge    xs ys) = rnf (xs, ys)
  rnf (Take      i xs) = rnf (i,  xs)
  rnf (Drop      i xs) = rnf (i,  xs)
  rnf (Sort      d xs) = rnf (d,  xs)
  rnf (Nub         xs) = rnf xs

instance Foldable List where
  foldMap f = foldMap f . toUnorderedList

instance Ord a => Eq (List a) where
  a == b = compare a b == EQ

instance Ord a => Ord (List a) where
  compare = compare `on` toAscList

instance (Ord a, Show a) => Show (List a) where
  show = show . toAscList

instance Monoid (List a) where
  mempty  = empty
  mappend = merge

-- | A sorting direction, either ascending or descending.
data Direction = Asc | Desc
  deriving (Eq, Ord, Show)

instance NFData Direction where
  rnf Asc  = ()
  rnf Desc = ()

-------------------------------------------------------------------------------

-- | /O(1)/ Create an empty ordered list.

empty :: List a
empty = FromBoth S.empty S.empty

-- | /O(1)/ Create a singleton ordered list.

singleton :: a -> List a
singleton x = FromBoth (S.singleton x) (S.singleton x)

-- | /O(1)/ Add a single element to an ordered list.

add :: a -> List a -> List a
add x = Merge (singleton x)

-- | /O(1)/ Merge two ordered lists.

merge :: List a -> List a -> List a
merge a b | null a    = b
          | null b    = a
          | otherwise = Merge a b

-- | /O(m)/ Merge a collection of ordered lists.

merges :: Foldable f => f (List a) -> List a
merges = foldr merge empty

-- | /O(n)/ Map a function over all items in the ordered list monotonically.
-- The result must be ordered again otherwise the state of the list will be
-- undefined and observation will most likely fail. A new comparison function
-- must be specified to allow potential nub'ing of the new elements.

mapMonotonic :: Ord b => (a -> b) -> List a -> List b
mapMonotonic f = r where 
  r l = case l of
          From      d xs -> From d   (fmap f xs)
          FromBoth xs ys -> FromBoth (fmap f xs) (fmap f ys)
          Merge    xs ys -> Merge    (r xs) (r ys)
          Take      j xs -> Take j   (r xs)
          Drop      j xs -> Drop j   (r xs)
          Sort      d xs -> Sort d   (r xs)
          Nub         xs -> Nub      (r xs)

-- | /O(n * m)/ Map a function that produces ordered lists over every item and
-- merge the results into a new ordered list. This function is the monadic bind
-- for the ordered list monad.

mergeMap :: (a -> List b) -> List a -> List b
mergeMap f = merges . map f . toUnorderedList

-- | /O(n)/ Filter elements from the list.

filter :: (a -> Bool) -> List a -> List a
filter f (From      d xs) = From d   (S.filter f xs)
filter f (FromBoth xs ys) = FromBoth (S.filter f xs) (S.filter f ys)
filter f (Merge    xs ys) = Merge    (filter f xs) (filter f ys)
filter f (Take      j xs) = Take j   (filter f xs)
filter f (Drop      j xs) = Drop j   (filter f xs)
filter f (Sort      d xs) = Sort d   (filter f xs)
filter f (Nub         xs) = Nub      (filter f xs)

-- | /O(m)/ Take a fixed number of items from the beginning of the list.

take :: Int -> List a -> List a
take j = Take j

-- | /O(n)/ Drop a fixed number of items from the beginning of the list.

drop :: Int -> List a -> List a
drop i = Drop i

-- | /O(n)/ Compute the length of the list.

length :: List a -> Int
length = S.length . toUnorderedSeq

-- | /O(max n m)/ Compute the size of list, but never count more than /m/
-- items.

boundedLength :: Int -> List a -> Maybe Int
boundedLength n = smartLength n . toUnorderedSeq

-- | /O(1)/ Is the list empty?

null :: List a -> Bool
null = isJust . boundedLength 0

-- | /O(n)/ Remove duplicate items from the list.

nub :: Eq a => List a -> List a
nub = Nub

-- | /O(n)/ Sort the list in ascending or descending order.

sort :: Ord a => Direction -> List a -> List a
sort = Sort

-------------------------------------------------------------------------------

-- | /O(m * log m)/ Create an ordered list form a Haskell sequence. No
-- assumption is made about the order of the items of the input list, it will
-- be sorted before it is converted. When you know in advance the input list is
-- in a certain order use the 'fromAscSeq' or 'fromDescSeq'.

fromSeq :: Ord a => Seq a -> List a
fromSeq xs = let sorted = S.unstableSort xs in FromBoth sorted (S.reverse sorted)

-- | /O(1)/ Create an ordered list form a Haskell sequence that has all the
-- items already in ascending order.

fromAscSeq :: Seq a -> List a
fromAscSeq = From Asc

-- | /O(1)/ Create an ordered list form a Haskell sequence that has all the
-- items already in descending order.

fromDescSeq :: Seq a -> List a
fromDescSeq = From Desc

-- | /O(1)/ Create an ordered list form two Haskell sequence that have all the
-- items already in ascending and respectively descending order. The invariant
-- should hold that the second input sequence is the reverse of the first input
-- sequence.

fromAscOrDescSeq :: Seq a -> Seq a -> List a
fromAscOrDescSeq = FromBoth

-- | /O(m)/ Create an ordered list form a list of potentially unordered Haskell
-- Sequences. 

fromSeqs :: Ord a => [Seq a] -> List a
fromSeqs = fromSeq . mconcat

-------------------------------------------------------------------------------

-- | /O(m * log m)/ Like `fromSeq' but construct from a regular Haskell list.

fromList :: Ord a => [a] -> List a
fromList = fromSeq . S.fromList

-- | /O(1)/ Like `fromAscSeq' but construct from a regular Haskell list.

fromAscList :: [a] -> List a
fromAscList = fromAscSeq . S.fromList

-- | /O(1)/ Like `fromDescSeq' but construct from a regular Haskell list.

fromDescList :: [a] -> List a
fromDescList = fromDescSeq . S.fromList

-- | /O(1)/ Like `fromAscOrDescSeq' but construct from regular Haskell lists.

fromAscOrDescList :: [a] -> [a] -> List a
fromAscOrDescList xs ys = fromAscOrDescSeq (S.fromList xs) (S.fromList ys)

-- | /O(m)/ Like `fromSeqs' but construct from regular Haskell lists.

fromLists :: Ord a => [[a]] -> List a
fromLists = fromSeqs . map S.fromList

-------------------------------------------------------------------------------

fromMap :: M.Map k a -> List a
fromMap m = fromAscOrDescList (map snd (M.toAscList m)) (map snd (M.toDescList m))

fromMapRange :: Ord k => Maybe k -> Maybe k -> M.Map k a -> List a
fromMapRange a b = fromMap . mapRange a b

-------------------------------------------------------------------------------

-- | /O(n * n)/ Observe the ordered list as a Haskell sequence. The Sequence
-- can be constructed with all elements in ascending order, in descending
-- order, or without any specific ordering.
-- 
-- /A note on performance:/ The quadratic running time is worst-case and only
-- holds when the list is constructed out of multiple single-element items.
-- For ordered lists constructed out off a small number of large input lists
-- the running time will be approximately linear.

toSeq :: Ord a => Maybe Direction -> List a -> Seq a
toSeq Nothing     = toUnorderedSeq
toSeq (Just Desc) = toDescSeq
toSeq (Just Asc)  = toAscSeq

-- | /O(n)/ Observe the ordered list as an unordered Haskell sequence.

toUnorderedSeq :: List a -> Seq a
toUnorderedSeq (From Asc  xs   ) = xs
toUnorderedSeq (From Desc xs   ) = xs
toUnorderedSeq (FromBoth  xs _ ) = xs
toUnorderedSeq (Merge     xs ys) = toUnorderedSeq xs `mappend` toUnorderedSeq ys
toUnorderedSeq (Take       j xs) = S.take j (toUnorderedSeq xs)
toUnorderedSeq (Drop       j xs) = S.drop j (toUnorderedSeq xs)
toUnorderedSeq (Sort Asc     xs) = toAscSeq xs
toUnorderedSeq (Sort Desc    xs) = toDescSeq xs
toUnorderedSeq (Nub          xs) = localNubBy (==) (toUnorderedSeq xs)

-- | /O(n * n)/ Observe the ordered list as an ordered Haskell sequence with
-- items in ascending order.

toAscSeq :: Ord a => List a -> Seq a
toAscSeq (From Asc  xs   ) = xs
toAscSeq (From Desc xs   ) = S.reverse xs
toAscSeq (FromBoth  xs _ ) = xs
toAscSeq (Merge     xs ys) = mergeBy (>) (toAscSeq xs) (toAscSeq ys)
toAscSeq (Take       j xs) = S.take j (toAscSeq xs)
toAscSeq (Drop       j xs) = S.drop j (toAscSeq xs)
toAscSeq (Sort Asc     xs) = toAscSeq xs
toAscSeq (Sort Desc    xs) = toAscSeq xs
toAscSeq (Nub          xs) = localNubBy (==) (toAscSeq xs)

-- | /O(n * n)/ Observe the ordered list as an ordered Haskell sequence with
-- items in descending order.

toDescSeq :: Ord a => List a -> Seq a
toDescSeq (From Asc  xs   ) = S.reverse xs
toDescSeq (From Desc xs   ) = xs
toDescSeq (FromBoth  _  ys) = ys
toDescSeq (Merge     xs ys) = mergeBy (<) (toDescSeq xs) (toDescSeq ys)
toDescSeq (Take       j xs) = S.take j (toDescSeq xs)
toDescSeq (Drop       j xs) = S.drop j (toDescSeq xs)
toDescSeq (Sort Asc     xs) = toDescSeq xs
toDescSeq (Sort Desc    xs) = toDescSeq xs
toDescSeq (Nub          xs) = localNubBy (==) (toDescSeq xs)

-------------------------------------------------------------------------------

-- | /O(n * n)/ Like `toSeq' but convert to a regular Haskell list.

toList :: Ord a => Maybe Direction -> List a -> [a]
toList a = F.toList . toSeq a

-- | /O(n)/ Like `toUnorderedSeq' but convert to a regular Haskell list.

toUnorderedList :: List a -> [a]
toUnorderedList = F.toList . toUnorderedSeq

-- | /O(n * n)/ Like `toAscSeq' but convert to a regular Haskell list.

toAscList :: Ord a => List a -> [a]
toAscList = F.toList . toAscSeq

-- | /O(n * n)/ Like `toDescSeq' but convert to a regular Haskell list.

toDescList :: Ord a => List a -> [a]
toDescList = F.toList . toDescSeq

-------------------------------------------------------------------------------

-- Helper functions.

mergeBy :: (a -> a -> Bool) -> Seq a -> Seq a -> Seq a
mergeBy f = m
  where m as bs = case viewl as of
                    EmptyL    -> bs
                    (x :< xs) -> let (a, b) = breakl (`f` x) bs
                                 in mappend a (x <| m b xs)

localNubBy :: (a -> a -> Bool) -> Seq a -> Seq a
localNubBy f = n
  where n zs = case viewl zs of
                 EmptyL  -> zs
                 x :< xs ->
                   case viewl xs of
                     EmptyL              ->        zs
                     y :< _  | x `f` y   ->      n xs
                             | otherwise -> x <| n xs

mapRange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> M.Map k v
mapRange from to m0 =
  let (e0, m1) = case from of Nothing -> (Nothing, m0)
                              Just f  -> let (_, m, o) = M.splitLookup f m0 in (m, o)
      (e1, m2) = case to   of Nothing -> (Nothing, m1)
                              Just f  -> let (o, m, _) = M.splitLookup f m1 in (m, o)
  in case (M.insert <$> from <*> e0, M.insert <$> to <*> e1) of
       (Just f, Just g) -> f (g m2)
       (Just f, _     ) -> f m2
       (_     , Just g) -> g m2
       (_     , _     ) -> m2

-- | Lazily compute the length of a list, never count any further than the
-- input bound. A result of Right means the length result is exact, a Left
-- result means at least that many items are contained.

smartLength :: Num i => i -> Seq a -> Maybe i
smartLength a b = f a b
  where f n zs = case (n, viewl zs) of
                   (_, EmptyL ) -> Just 0
                   (0, _      ) -> Nothing
                   (m, _ :< xs) -> fmap (1 +) (f (m - 1) xs)

-------------------------------------------------------------------------------

newtype ListT m a = ListT { runListT :: m (List a) }

instance Monad m => Monad (ListT m) where
  return a = ListT (return (singleton a))
  m >>= k  = ListT $
    do a <- runListT m
       merges `liftM` mapM (runListT . k) (toUnorderedSeq a)

instance MonadTrans ListT where
   lift m = ListT (singleton `liftM` m)

instance MonadIO m => MonadIO (ListT m) where
   liftIO = lift . liftIO

instance Monad m => MonadPlus (ListT m) where
  mzero       = ListT (return empty)
  m `mplus` n = ListT (liftM2 merge (runListT m) (runListT n))

instance Monad m => Functor (ListT m) where
  fmap = liftM

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

instance Monad List where
  return = singleton
  (>>=)  = flip mergeMap

instance MonadPlus List where
  mzero = empty
  mplus = merge

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Alternative List where
  empty = mzero
  (<|>) = mplus

