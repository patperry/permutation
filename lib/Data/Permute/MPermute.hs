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
-- "Data.Array.ST".
--

module Data.Permute.MPermute (
    -- * Class of mutable permutation types
    HasPermuteArray(..),
    PermuteData,
    MPermute(..),
    
    -- * Constructing mutable permutations
    newListPermute,
    newSwapsPermute,
    newCopyPermute,
    copyPermute,
    setIdentity,
    
    -- * Accessing permutation elements
    getPermute,
    unsafeGetPermute,
    swapPermute,
    
    -- * Permutation properties
    getElems,
    getElems',
    isValid,

    -- * Permutation functions
    getInverse,
    copyInverse,
    setNext,
    setPrev,
    
    -- * Applying permutations
    getSwaps,
    getInvSwaps,
    
    -- * Converstions between mutable and immutable permutations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
    
    ) where

import Data.Permute.Internal