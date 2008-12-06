-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.MPermute
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- An overloaded interface to mutable permutations. For permutation types 
-- which can be used with this interface, see "Data.Permute.IO" and
-- "Data.Permute.ST".
--

module Data.Permute.MPermute (
    -- * Class of mutable permutation types
    MPermute,
    
    -- * Constructing mutable permutations
    newPermute,
    newPermute_,
    newListPermute,
    unsafeNewListPermute,
    newInvSwapsPermute,
    unsafeNewInvSwapsPermute,
    newCopyPermute,
    copyPermute,
    setIdentity,
    
    -- * Accessing permutation elements
    getElem,
    unsafeGetElem,
    swapElems,
    unsafeSwapElems,
    
    -- * Permutation properties
    getSize,
    getElems,
    isValid,

    -- * Permutation functions
    getInverse,
    copyInverse,
    setNext,
    setPrev,
    
    -- * Applying permutations
    getSwaps,
    getInvSwaps,
    
    -- * Sorting
    getOrder,
    getOrderBy,
    getRank,
    getRankBy,
    
    -- * Converstions between mutable and immutable permutations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
    
    ) where

import Data.Function( on )
import Data.List( sortBy )

import Data.Permute.Internal


-- | Returns a permutation which rearranges its first argument into ascending 
-- order.  This is a special case of 'getOrderBy'.
getOrder :: (Ord a, MPermute p m) => [a] -> m p
getOrder = getOrderBy compare

getOrderBy :: (MPermute p m) => (a -> a -> Ordering) -> [a] -> m p
getOrderBy cmp xs =
    let is = (fst . unzip . sortBy (cmp `on` snd) . zip [0..]) xs
        n  = length xs
    in newListPermute n is

-- | Returns a permutation, the inverse of which rearranges its first argument 
-- into ascending order. The returned permutation, @p@, has the property that
-- @p[i]@ is the rank of the @i@th element of the passed-in list. This is a 
-- special case of 'getRankBy'.
getRank :: (Ord a, MPermute p m) => [a] -> m p
getRank = getRankBy compare

getRankBy :: (MPermute p m) => (a -> a -> Ordering) -> [a] -> m p
getRankBy cmp xs = do
    p <- getOrderBy cmp xs
    getInverse p
