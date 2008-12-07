{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Immutable permutations.

module Data.Permute (
    -- * Permutations
    Permute,
    
    -- * Creating permutations
    permute,
    listPermute,
    swapsPermute,

    -- * Accessing permutation elements
    apply,
    unsafeApply,

    -- * Permutation properties
    size,
    elems,
    
    -- * Permutation functions
    inverse,
    next,
    prev,
    
    -- * Applying permutations
    swaps,
    invSwaps,
    
    -- * Sorting
    sort,
    sortBy,
    order,
    orderBy,
    rank,
    rankBy,
    
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Permute.Base
import Data.Permute.ST

-- | Construct an identity permutation of the given size.
permute :: Int -> Permute
permute n = runST $
    unsafeFreeze =<< newPermute n

-- | Construct a permutation from a list of elements.  
-- @listPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
listPermute :: Int -> [Int] -> Permute
listPermute n is = runST $
    unsafeFreeze =<< newListPermute n is

-- | Construct a permutation from a list of swaps.
-- @swapsPermute n ss@ creats a permuation of size @n@ given by a
-- sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
swapsPermute :: Int -> [(Int,Int)] -> Permute
swapsPermute n ss = runST $
    unsafeFreeze =<< newSwapsPermute n ss

-- | @apply p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
apply :: Permute -> Int -> Int
apply p i
    | i >= 0 && i < size p = 
        unsafeApply p i
    | otherwise =
        error "Invalid index"
{-# INLINE apply #-}

-- | Get the inverse of a permutation
inverse :: Permute -> Permute
inverse p = runST $ 
    unsafeFreeze =<< getInverse =<< unsafeThaw p

-- | Return the next permutation in lexicographic order, or @Nothing@ if
-- there are no further permutations.  Starting with the identity permutation
-- and repeatedly calling this function will iterate through all permutations
-- of a given order.
next :: Permute -> Maybe Permute
next = nextPrevHelp setNext

-- | Return the previous permutation in lexicographic order, or @Nothing@
-- if there is no such permutation.
prev :: Permute -> Maybe Permute
prev = nextPrevHelp setPrev

nextPrevHelp :: (forall s. STPermute s -> ST s Bool) 
             -> Permute -> Maybe Permute
nextPrevHelp set p = runST $ do
    p' <- thaw p
    set p' >>= \valid ->
        if valid
            then liftM Just $ unsafeFreeze p'
            else return Nothing

-- | Get a list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.
swaps :: Permute -> [(Int,Int)]
swaps p = runST $
    getSwaps =<< unsafeThaw p

-- | Get a list of swaps equivalent to the inverse of permutation.
invSwaps :: Permute -> [(Int,Int)]
invSwaps p = runST $
    getInvSwaps =<< unsafeThaw p

-- | Sorts a list and returns a permutation with transforms the original list
-- into sorted order.  This is a special case of 'sortBy'.
sort :: (Ord a) => [a] -> ([a], Permute)
sort xs = runST $ do
    (xs',mp) <- getSort xs
    p <- unsafeFreeze mp
    return (xs',p)
    
sortBy :: (a -> a -> Ordering) -> [a] -> ([a], Permute)
sortBy cmp xs = runST $ do
    (xs',mp) <- getSortBy cmp xs
    p <- unsafeFreeze mp
    return (xs',p)

-- | Returns a permutation which rearranges its first argument into ascending 
-- order.  This is a special case of 'orderBy'.
order :: (Ord a) => [a] -> Permute
order xs = runST $ 
    unsafeFreeze =<< getOrder xs

orderBy :: (a -> a -> Ordering) -> [a] -> Permute
orderBy cmp xs = runST $
    unsafeFreeze =<< getOrderBy cmp xs

-- | Returns a permutation, the inverse of which rearranges its first argument 
-- into ascending order. The returned permutation, @p@, has the property that
-- @p[i]@ is the rank of the @i@th element of the passed-in list. This is a 
-- special case of 'rankBy'.
rank :: (Ord a) => [a] -> Permute
rank xs = runST $
    unsafeFreeze =<< getRank xs

rankBy :: (a -> a -> Ordering) -> [a] -> Permute
rankBy cmp xs = runST $
    unsafeFreeze =<< getRankBy cmp xs
