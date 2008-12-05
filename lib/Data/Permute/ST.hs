{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.ST
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Mutable permutations in the 'ST' monad.

module Data.Permute.ST (
    -- * Permutations
    STPermute,
    runSTPermute,
    
    -- * Overloaded mutable permutation interface
    module Data.Permute.MPermute
    ) where

import Control.Monad.ST

import Data.Permute.Internal
import Data.Permute.MPermute

-- | A safe way to create and work with a mutable permutation before returning 
-- an immutable permutation for later perusal. This function avoids copying the 
-- permutation before returning it - it uses unsafeFreeze internally, but this 
-- wrapper is a safe interface to that function. 
runSTPermute :: (forall s. ST s (STPermute s)) -> Permute
runSTPermute p = runST (p >>= unsafeFreeze)
