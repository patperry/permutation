{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose.ST
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Mutable combinations in the 'ST' monad.

module Data.Choose.ST (
    -- * Combinations
    STChoose,
    runSTChoose,
    
    -- * Overloaded mutable combination interface
    module Data.Choose.MChoose
    ) where

import Control.Monad.ST

import Data.Choose.Base( Choose, STChoose, unsafeFreezeSTChoose )
import Data.Choose.MChoose

-- | A safe way to create and work with a mutable combination before returning 
-- an immutable one for later perusal. This function avoids copying the 
-- combination before returning it - it uses unsafeFreeze internally, but this 
-- wrapper is a safe interface to that function. 
runSTChoose :: (forall s. ST s (STChoose s)) -> Choose
runSTChoose c = runST (c >>= unsafeFreezeSTChoose)
{-# INLINE runSTChoose #-}
