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
    invSwapsPermute,

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
    order,
    orderBy,
    rank,
    rankBy,
    
    ) where

import Control.Monad.ST
import Data.Permute.Base
import Data.Permute.ST

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

