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
    
    -- * Converstions between mutable and immutable permutations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
    
    ) where

import Data.Permute.Internal
