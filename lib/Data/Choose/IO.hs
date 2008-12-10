-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose.IO
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Mutable combinations in the 'IO' monad.

module Data.Choose.IO (
    -- * Combinations
    IOChoose,
    
    -- * Overloaded mutable combination interface
    module Data.Choose.MChoose
    ) where

import Data.Choose.IOBase( IOChoose )
import Data.Choose.MChoose
