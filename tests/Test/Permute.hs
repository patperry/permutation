{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Test.Permute
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- An 'Arbitrary' instance and functions for generating random permutations.
--

module Test.Permute (
    permute
    ) where

import Data.List ( sortBy )
import Data.Ord ( comparing )

import Test.QuickCheck

import Data.Permute ( Permute )
import qualified Data.Permute as P

-- | Generate a random permutation of the given size.
permute :: Int -> Gen Permute
permute n = do
    xs <- vector n :: Gen [Int]
    let is = (snd . unzip) $ sortBy (comparing fst) $ zip xs [0..]
    return $ P.listPermute n is


instance Arbitrary Permute where
    arbitrary = do
        n <- arbitrary >>= return . abs
        p <- permute n
        return $ p
        
    coarbitrary p =
        coarbitrary $ P.elems p
