{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , EmptyDataDecls
  , GADTs
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  #-}

{- |
Ordered List datatype with efficient and streaming observation in two
directions. This list is especially useful when constructing ordered sequences
from a collection of preordered sub-sequences.
-}
module Data.List.Ordered
  (
  -- * Ordered list type.
    List
  , Order (..)

  -- * Constructing ordered lists.
  , empty
  , singleton
  , add
  , append
  , concat

  -- * Operations on ordered lists.
  , null
  , length
  , mapMonotonic
  , mergeMap
  , filter
  , take
  , drop
  , boundedLength
  , uniq
  , sort
  , sortBy

  -- * Creation from Haskell lists.
  , fromList
  , fromAscList
  , fromDescList
  , fromAscOrDescList
  , fromLists

  -- * Creation from Maps.
  , fromMap
  , fromMapRange

  -- * Observing as regular Haskell lists.
  , toList
  , toUnorderedList
  , toAscList
  , toDescList

  -- * List monad transformer wrapper.
  , ListT (..)

  -- * Internally used helper functions.
  , mergeBy
  , uniquesBy
  , mapRange
  , smartLength
  ) where

import Prelude hiding (concat, drop, filter, foldr, length, mapM, null, take)

import Control.Applicative hiding (empty)
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Trans
import Data.Foldable (Foldable (foldMap))
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable (mapM)
import qualified Control.Applicative
import qualified Data.List           as L
import qualified Data.Map.Strict     as M

-------------------------------------------------------------------------------

data List a  where
  Directed ::          Order -> [a]    -> List a
  FromBoth ::          [a] -> [a]      -> List a
  Merge    ::          [List a]        -> List a
  Take     ::          Int -> List a   -> List a
  Drop     ::          Int -> List a   -> List a
  Sort     :: Ord a => Order -> List a -> List a
  Uniq     :: Eq a  => List a          -> List a

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
  mappend = append
  mconcat = concat

-- | A sorting direction, either ascending or descending.
data Order = Asc | Desc
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

-- | /O(1)/ Create an empty ordered list.

empty :: List a
empty = FromBoth [] []

-- | /O(1)/ Create a singleton ordered list.

singleton :: a -> List a
singleton x = let s = [x] in FromBoth s s

-- | /O(1)/ Add a single element to an ordered list.

add :: a -> List a -> List a
add x = append (singleton x)

-- | /O(1)/ Merge two ordered lists.

append :: List a -> List a -> List a
append a          b | null a = b
                    | null b = a
append (Merge xs) (Merge ys) = Merge (xs ++ ys)
append (Merge xs) b          = Merge (xs ++ [b])
append a          (Merge ys) = Merge (a : ys)
append a          b          = Merge [a, b]

-- | /O(m)/ Merge a collection of ordered lists.

concat :: [List a] -> List a
concat = Merge

-- | /O(n)/ Map a function over all items in the ordered list monotonically.
-- The result must be in order again, otherwise the state of the list will be
-- undefined and ordered observation will most likely fail. A new comparison
-- function must be specified to allow potential unique'ing of the new
-- elements.

mapMonotonic :: Ord b => (a -> b) -> List a -> List b
mapMonotonic f = r where
  r l = case l of
          Directed  d xs -> Directed d (fmap f xs)
          FromBoth xs ys -> FromBoth (fmap f xs) (fmap f ys)
          Merge       xs -> Merge    (fmap r xs)
          Take      j xs -> Take j   (r xs)
          Drop      j xs -> Drop j   (r xs)
          Sort      d xs -> Sort d   (r xs)
          Uniq        xs -> Uniq     (r xs)

-- | /O(n * m)/ Map a function that produces ordered lists over every item and
-- append the results into a new ordered list. This function is the monadic bind
-- for the ordered list monad.

mergeMap :: (a -> List b) -> List a -> List b
mergeMap f = concat . fmap f . toUnorderedList

-- | /O(n)/ Filter elements from the list.

filter :: (a -> Bool) -> List a -> List a
filter f (Directed  d xs) = Directed d (L.filter f xs)
filter f (FromBoth xs ys) = FromBoth (L.filter f xs) (L.filter f ys)
filter f (Merge       xs) = Merge    (fmap (filter f) xs)
filter f (Take      j xs) = Take j   (filter f xs)
filter f (Drop      j xs) = Drop j   (filter f xs)
filter f (Sort      d xs) = Sort d   (filter f xs)
filter f (Uniq        xs) = Uniq     (filter f xs)

-- | /O(m)/ Take a fixed number of items from the beginning of the list.

take :: Int -> List a -> List a
take j = Take j

-- | /O(n)/ Drop a fixed number of items from the beginning of the list.

drop :: Int -> List a -> List a
drop i = Drop i

-- | /O(n)/ Compute the length of the list.

length :: List a -> Int
length = L.length . toUnorderedList

-- | /O(max n m)/ Compute the size of list, but never count more than /m/
-- items.

boundedLength :: Int -> List a -> Maybe Int
boundedLength n = smartLength n . toUnorderedList

-- | /O(1)/ Is the list empty?

null :: List a -> Bool
null = isJust . boundedLength 0

-- | /O(n)/ Remove duplicate items from the list.

uniq :: Eq a => List a -> List a
uniq = Uniq

-- | Sort the list in ascending or descending order using the default `Ord'
-- instance for the value type.

sort :: Ord a => Order -> List a -> List a
sort = Sort

-- | Sort the list in ascending or descending order with a custom ordering
-- function. This breaks the original list structure and rebuild a new order
-- list from the result. This is likely to be inefficient.

sortBy :: Order -> (a -> a -> Ordering) -> List a -> List a
sortBy d f =
  case d of
    Asc  -> fromAscList  . L.sortBy f
    Desc -> fromDescList . L.sortBy (flip f)
  . toUnorderedList

-------------------------------------------------------------------------------

-- | /O(m * log m)/

fromList :: Ord a => [a] -> List a
fromList xs =
  let sorted = L.sort xs
  in FromBoth sorted (L.reverse sorted)

-- | /O(1)/

fromAscList :: [a] -> List a
fromAscList = Directed Asc

-- | /O(1)/

fromDescList :: [a] -> List a
fromDescList = Directed Desc

-- | /O(1)/

fromAscOrDescList :: [a] -> [a] -> List a
fromAscOrDescList = FromBoth

-- | /O(m)/

fromLists :: Ord a => [[a]] -> List a
fromLists = fromList . mconcat

-------------------------------------------------------------------------------

fromMap :: M.Map k a -> List a
fromMap m = fromAscOrDescList (map snd (M.toAscList m)) (map snd (M.toDescList m))

fromMapRange :: Ord k => Maybe k -> Maybe k -> M.Map k a -> List a
fromMapRange a b = fromMap . mapRange a b

-------------------------------------------------------------------------------

-- | /O(n * n)/ Observe the ordered list as an ordinary Haskell list. The list
-- can be constructed with all elements in ascending order, in descending
-- order, or without any specific ordering.
--
-- /A note on performance:/ The quadratic running time is worst-case and only
-- holds when the list is constructed out of multiple single-element items.
-- For ordered lists constructed out off a small number of large input lists
-- the running time will be approximately linear.

toList :: Ord a => Maybe Order -> List a -> [a]
toList Nothing     = toUnorderedList
toList (Just Desc) = toDescList
toList (Just Asc)  = toAscList

-- | /O(n)/ Observe the ordered list as an unordered Haskell list.

toUnorderedList :: List a -> [a]
toUnorderedList (Directed Asc  xs) = xs
toUnorderedList (Directed Desc xs) = xs
toUnorderedList (FromBoth   xs _ ) = xs
toUnorderedList (Merge         xs) = mconcat (fmap toUnorderedList xs)
toUnorderedList (Take        j xs) = L.take j (toUnorderedList xs)
toUnorderedList (Drop        j xs) = L.drop j (toUnorderedList xs)
toUnorderedList (Sort Asc      xs) = toAscList xs
toUnorderedList (Sort Desc     xs) = toDescList xs
toUnorderedList (Uniq          xs) = uniquesBy (==) (toUnorderedList xs)

-- | /O(n * n)/ Observe the ordered list as an ordered Haskell list with
-- items in ascending order.

toAscList :: Ord a => List a -> [a]
toAscList (Directed Asc  xs) = xs
toAscList (Directed Desc xs) = reverse xs
toAscList (FromBoth   xs _ ) = xs
toAscList (Merge         xs) = mergeBy compare (fmap toAscList xs)
toAscList (Take        j xs) = L.take j (toAscList xs)
toAscList (Drop        j xs) = L.drop j (toAscList xs)
toAscList (Sort Asc      xs) = toAscList xs
toAscList (Sort Desc     xs) = toAscList xs
toAscList (Uniq          xs) = uniquesBy (==) (toAscList xs)

-- | /O(n * n)/ Observe the ordered list as an ordered Haskell list with
-- items in descending order.

toDescList :: Ord a => List a -> [a]
toDescList (Directed Asc  xs) = reverse xs
toDescList (Directed Desc xs) = xs
toDescList (FromBoth   _  ys) = ys
toDescList (Merge         xs) = mergeBy (flip compare) (fmap toDescList xs)
toDescList (Take        j xs) = L.take j (toDescList xs)
toDescList (Drop        j xs) = L.drop j (toDescList xs)
toDescList (Sort Asc      xs) = toDescList xs
toDescList (Sort Desc     xs) = toDescList xs
toDescList (Uniq          xs) = uniquesBy (==) (toDescList xs)

-------------------------------------------------------------------------------

-- Helper functions.

-- | Merge a series of ordered lists into a single list. This defintion is a
-- direct copied from the standard Haskell Data.List.sortBy.

mergeBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeBy cmp = mergeAll
  where
    mergeAll []         = []
    mergeAll [x]        = x
    mergeAll xs         = mergeAll (mergePairs xs)
    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs
    merge as@(a:as')
          bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

-- | Remove all consecutive duplicates from the input list.

uniquesBy :: (a -> a -> Bool) -> [a] -> [a]
uniquesBy f = n
  where
    n zs =
      case zs of
        []   -> zs
        _:[] -> zs
        x:xs@(y:_)
          | x `f` y   ->     n xs
          | otherwise -> x : n xs

-- | Select a range from Map.

mapRange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> M.Map k v
mapRange from to m0 =
  let (e0, m1) = case from of
                   Nothing -> (Nothing, m0);
                   Just f  -> let (_, m, o) = M.splitLookup f m0 in (m, o)
      (e1, m2) = case to of
                   Nothing -> (Nothing, m1)
                   Just f  -> let (o, m, _) = M.splitLookup f m1 in (m, o)
  in case (M.insert <$> from <*> e0, M.insert <$> to <*> e1) of
       (Just f, Just g) -> f (g m2)
       (Just f, _     ) -> f m2
       (_     , Just g) -> g m2
       (_     , _     ) -> m2

-- | Lazily compute the length of a list, never count any further than the
-- input bound. A result of Right means the length result is exact, a Left
-- result means at least that many items are contained.

smartLength :: (Eq i, Num i) => i -> [a] -> Maybe i
smartLength a b = f a b
  where f n zs = case (n, zs) of
                   (_, []    ) -> Just 0
                   (0, _     ) -> Nothing
                   (m, _ : xs) -> fmap (1 +) (f (m - 1) xs)

-------------------------------------------------------------------------------

newtype ListT m a = ListT { runListT :: m (List a) }

instance Monad m => Monad (ListT m) where
  return a = ListT (return (singleton a))
  m >>= k  = ListT $
    do a <- runListT m
       concat `liftM` mapM (runListT . k) (toUnorderedList a)

instance MonadTrans ListT where
   lift m = ListT (singleton `liftM` m)

instance MonadIO m => MonadIO (ListT m) where
   liftIO = lift . liftIO

instance Monad m => MonadPlus (ListT m) where
  mzero       = ListT (return empty)
  m `mplus` n = ListT (liftM2 append (runListT m) (runListT n))

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
  mplus = append

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Alternative List where
  empty = mzero
  (<|>) = mplus

