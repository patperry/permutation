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
    at,
    unsafeAt,

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
-- @listPermute n is@ creates a permutation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
listPermute :: Int -> [Int] -> Permute
listPermute n is = runST $
    unsafeFreeze =<< newListPermute n is

-- | Construct a permutation from a list of swaps.
-- @swapsPermute n ss@ creats a permutation of size @n@ given by a
-- sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
swapsPermute :: Int -> [(Int,Int)] -> Permute
swapsPermute n ss = runST $
    unsafeFreeze =<< newSwapsPermute n ss

-- | @at p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
at :: Permute -> Int -> Int
at p i
    | i >= 0 && i < size p = 
        unsafeAt p i
    | otherwise =
        error "Invalid index"
{-# INLINE at #-}

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


-- | @sort n xs@ sorts the first @n@ elements of @xs@ and returns a 
-- permutation which transforms @xs@ into sorted order.  The results are
-- undefined if @n@ is greater than the length of @xs@.  This is a special 
-- case of 'sortBy'.
sort :: (Ord a) => Int -> [a] -> ([a], Permute)
sort n xs = runST $ do
    (xs',mp) <- getSort n xs
    p <- unsafeFreeze mp
    return (xs',p)
    
sortBy :: (a -> a -> Ordering) -> Int -> [a] -> ([a], Permute)
sortBy cmp n xs = runST $ do
    (xs',mp) <- getSortBy cmp n xs
    p <- unsafeFreeze mp
    return (xs',p)


-- | @order n xs@ returns a permutation which rearranges the first @n@
-- elements of @xs@ into ascending order. The results are undefined if @n@ is 
-- greater than the length of @xs@.  This is a special case of 'orderBy'.
order :: (Ord a) => Int -> [a] -> Permute
order n xs = runST $ 
    unsafeFreeze =<< getOrder n xs

orderBy :: (a -> a -> Ordering) -> Int -> [a] -> Permute
orderBy cmp n xs = runST $
    unsafeFreeze =<< getOrderBy cmp n xs

-- | @rank n xs@ eturns a permutation, the inverse of which rearranges the 
-- first @n@ elements of @xs@ into ascending order. The returned permutation, 
-- @p@, has the property that @p[i]@ is the rank of the @i@th element of @xs@. 
-- The results are undefined if @n@ is greater than the length of @xs@.
-- This is a special case of 'rankBy'.  
rank :: (Ord a) => Int -> [a] -> Permute
rank n xs = runST $
    unsafeFreeze =<< getRank n xs

rankBy :: (a -> a -> Ordering) -> Int -> [a] -> Permute
rankBy cmp n xs = runST $
    unsafeFreeze =<< getRankBy cmp n xs
