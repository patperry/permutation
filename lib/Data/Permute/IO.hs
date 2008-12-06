-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.IO
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Mutable permutations in the 'IO' monad.

module Data.Permute.IO (
    -- * Permutations
    IOPermute,
    
    -- * Overloaded mutable permutation interface
    module Data.Permute.MPermute
    ) where

import Data.Permute.IOBase( IOPermute )
import Data.Permute.MPermute
