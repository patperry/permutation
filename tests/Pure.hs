{-# LANGUAGE ScopedTypeVariables #-}
module Pure (
    tests_Permute
    ) where
    
import Control.Monad.ST
import Data.List( foldl' )
import Data.Permute
import Data.Array.ST

import Driver
import Test.QuickCheck
    
import Test.Permute()
import qualified Test.Permute as Test


prop_size_permute (Nat n) =
    size (permute n) == n
prop_elems_permute (Nat n) =
    elems (permute n) == [0..(n-1)]

prop_size_listPermute (ListPermute n is) =
    size (listPermute n is) == n
prop_elems_listPermute (ListPermute n is) =
    elems (listPermute n is) == is

prop_size_invSwapsPermute (InvSwapsPermute n ss) =
    size (invSwapsPermute n ss) == n
prop_elems_invSwapsPermute (InvSwapsPermute n ss) =
    elems (invSwapsPermute n ss) == map at [0..(n-1)]
  where
    at i = foldl' doSwap i $ reverse ss
    doSwap k (i,j) | k == i    = j
                   | k == j    = i
                   | otherwise = k

prop_apply       = prop_apply_help apply
prop_unsafeApply = prop_apply_help unsafeApply
prop_apply_help a =
    forAll arbitrary $ \(Index n i) ->
    forAll (Test.permute n) $ \p ->
        a p i == (elems p) !! i

prop_size_inverse (p :: Permute) =
    size (inverse p) == size p
prop_elems_inverse (p :: Permute) =
    all (\i -> is' !! (apply p i) == i) [0..(n-1)]
  where
    n   = size p
    is' = elems (inverse p)

prop_swaps (Nat n) =
    forAll (Test.permute n) $ \p ->
    forAll (vector n) $ \xs ->
        let xs' = applySwaps (swaps p) xs
        in all (\i -> xs' !! i == xs !! (apply p i)) [0..(n-1)]

prop_invSwaps (Nat n) =
    forAll (Test.permute n) $ \p ->
    forAll (vector n) $ \xs ->
        let xs' = applySwaps (invSwaps p) xs
        in all (\i -> xs' !! (apply p i) == xs !! i) [0..(n-1)]
    

tests_Permute = 
    [ ("size . permute"          , mytest prop_size_permute)
    , ("elems . permute"         , mytest prop_elems_permute)
    , ("size . listPermute"      , mytest prop_size_listPermute)
    , ("elems . listPermute"     , mytest prop_elems_listPermute)
    , ("size . invSwapsPermute"  , mytest prop_size_invSwapsPermute)
    , ("elems . invSwapsPermute" , mytest prop_elems_invSwapsPermute)
    , ("apply"                   , mytest prop_apply)
    , ("unsafeApply"             , mytest prop_unsafeApply)
    , ("size . inverse"          , mytest prop_size_inverse)
    , ("elems . inverse"         , mytest prop_elems_inverse)
    , ("swaps"                   , mytest prop_swaps)
    , ("invSwaps"                , mytest prop_invSwaps)
    ]


applySwaps :: [(Int,Int)] -> [Int] -> [Int]
applySwaps ss xs = runST $ do
    arr <- newListArray (0,length xs - 1) xs :: ST s (STUArray s Int Int) 
    mapM_ (swap arr) ss
    getElems arr
  where
    swap arr (i,j) = do
        i' <- readArray arr i
        j' <- readArray arr j
        writeArray arr j i'
        writeArray arr i j'

