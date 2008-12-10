{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Immutable combinations.

module Data.Choose (
    -- * Combinations
    Choose,
    
    -- * Creating combinations
    choose,
    listChoose,

    -- * Accessing combination elements
    at,
    unsafeAt,

    -- * Combination properties
    possible,
    size,
    elems,
    
    -- * Combination functions
    complement,
    complElems,
    next,
    prev,
    
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Choose.Base
import Data.Choose.ST

-- | @choose n k@ returns the first combination of @k@ outcomes from @n@
-- possibilites, namely the subset @{ 0, ..., k-1 }@.
choose :: Int -> Int -> Choose
choose n k = runST $
    unsafeFreeze =<< newChoose n k

-- | Construct a combination from a list of elements.  
-- @listChoose n k is@ creates a combination of @k@ outcomes from @n@
-- possibilities initialized to have the @i@th element equal to @is !! i@.  
-- For the combination to be valid, the elements must all be unique, they 
-- must be in sorted order, and they all must be in the range @0 .. n-1@.
listChoose :: Int -> Int -> [Int] -> Choose
listChoose n k is = runST $
    unsafeFreeze =<< newListChoose n k is

-- | @at c i@ gets the value of the @i@th element of the combination
-- @c@.  The index @i@ must be in the range @0..(k-1)@, where @k@ is the
-- size of the combination.
at :: Choose -> Int -> Int
at c i
    | i >= 0 && i < size c =
        unsafeAt c i
    | otherwise =
        error "Invalid index"
{-# INLINE at #-}

-- | Get the inverse of a combination
complement :: Choose -> Choose
complement c = runST $ 
    unsafeFreeze =<< getComplement =<< unsafeThaw c

-- | Get a list of the elements in the complement of a combination.
-- If the combination is a subset of @k@ outcomes from @n@ possibilities, then
-- the returned list will be sorted and of length @n-k@.  
complElems :: Choose -> [Int]
complElems c = runST $
    getComplElems =<< unsafeThaw c

-- | Return the next combination in lexicographic order, or @Nothing@ if
-- there are no further combinations.  Starting with the first combination
-- and repeatedly calling this function will iterate through all combinations
-- of a given order.
next :: Choose -> Maybe Choose
next = nextPrevHelp setNext

-- | Return the previous combination in lexicographic order, or @Nothing@
-- if such combination exists.
prev :: Choose -> Maybe Choose
prev = nextPrevHelp setPrev

nextPrevHelp :: (forall s. STChoose s -> ST s Bool) 
             -> Choose -> Maybe Choose
nextPrevHelp set c = runST $ do
    c' <- thaw c
    set c' >>= \valid ->
        if valid
            then liftM Just $ unsafeFreeze c'
            else return Nothing
