{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Test.Choose
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Test.Choose (
    choose
    ) where

import Control.Monad( liftM )
import Data.Ord( comparing )
import Data.List( sort, sortBy )
import Test.QuickCheck hiding ( choose )
import qualified Test.QuickCheck as QC
import Data.Choose( Choose )
import qualified Data.Choose as C

-- | @choose n k@ generates a random combination of @k@ outcomes
-- from @n@ possibilities.
choose :: Int -> Int -> Gen Choose
choose n k = do
    xs <- vector n :: Gen [Int]
    let is = (snd . unzip) $ sortBy (comparing fst) $ zip xs [0..]
    return $ C.listChoose n k $ sort $ take k is

instance Arbitrary Choose where
    arbitrary = do
        n <- liftM abs arbitrary
        k <- QC.choose (0,n)
        c <- choose n k
        return c
        
    coarbitrary c =
        coarbitrary $ (C.possible c, C.size c, C.elems c)
