{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Pure (
    tests_Permute
    ) where
    
import Control.Monad.ST
import Data.Array.ST
import Data.List( foldl' )
import qualified Data.List as List
import Data.Maybe( fromJust )

import Data.Permute

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

prop_size_swapsPermute (SwapsPermute n ss) =
    size (swapsPermute n ss) == n
prop_elems_swapsPermute (SwapsPermute n ss) =
    elems (swapsPermute n ss) == map at [0..(n-1)]
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

prop_swaps_inverse (Nat n) =
    forAll (Test.permute n) $ \p ->
    forAll (vector n) $ \xs ->
        applySwaps (swaps $ inverse p) xs == (applySwaps (invSwaps p) xs)
    
prop_invSwaps_inverse (Nat n) =
    forAll (Test.permute n) $ \p ->
    forAll (vector n) $ \xs ->
        applySwaps (invSwaps $ inverse p) xs == (applySwaps (swaps p) xs)

prop_prev_permute (Nat n) =
    prev (permute n) == Nothing
prop_next_last (Nat n) =
    next (listPermute n $ reverse [0..(n-1)]) == Nothing

prop_next_prev (p :: Permute) =
    case prev p of
        Just p' -> p == (fromJust $ next p')
        Nothing -> p == permute n
  where
    n = size p
    
prop_prev_next (p :: Permute) =
    case next p of
        Just p' -> p == (fromJust $ prev p')
        Nothing -> p == (listPermute n $ reverse [0..(n-1)])
  where
    n = size p

prop_fst_sort (Sort n xs) = let
    ys = take n xs
    in (fst . sort n) xs == (List.sort ys)
prop_snd_sort (Sort n xs) = let
    ys = take n xs
    in applySwaps (swaps $ snd $ sort n xs) ys == (List.sort ys)

prop_fst_sortBy (SortBy cmp n xs) = let 
    ys = take n xs
    in (fst . sortBy cmp n) xs == (List.sortBy cmp ys)
prop_snd_sortBy (SortBy cmp n xs) = let 
    ys = take n xs
    in applySwaps (swaps $ snd $ sortBy cmp n xs) ys == (List.sortBy cmp ys)

prop_order (Sort n xs) = let 
    ys = take n xs
    in applySwaps (swaps $ order n xs) ys == (List.sort ys)

prop_orderBy (SortBy cmp n xs) = let 
    ys = take n xs
    in applySwaps (swaps $ orderBy cmp n xs) ys == (List.sortBy cmp ys)

prop_rank (Sort n xs) = let
    ys = take n xs
    in applySwaps (invSwaps $ rank n xs) ys == (List.sort ys)

prop_rankBy (SortBy cmp n xs) = let
    ys = take n xs
    in applySwaps (invSwaps $ rankBy cmp n xs) ys == (List.sortBy cmp ys)

prop_swapsPermute_swaps (p :: Permute) =
    swapsPermute (size p) (swaps p) == p


tests_Permute = 
    [ ("size . permute"          , mytest prop_size_permute)
    , ("elems . permute"         , mytest prop_elems_permute)
    , ("size . listPermute"      , mytest prop_size_listPermute)
    , ("elems . listPermute"     , mytest prop_elems_listPermute)
    , ("size . swapsPermute"     , mytest prop_size_swapsPermute)
    , ("elems . swapsPermute"    , mytest prop_elems_swapsPermute)
    , ("apply"                   , mytest prop_apply)
    , ("unsafeApply"             , mytest prop_unsafeApply)
    , ("size . inverse"          , mytest prop_size_inverse)
    , ("elems . inverse"         , mytest prop_elems_inverse)
    , ("swaps"                   , mytest prop_swaps)
    , ("invSwaps"                , mytest prop_invSwaps)
    , ("swaps . inverse"         , mytest prop_swaps_inverse)
    , ("invSwaps . inverse"      , mytest prop_invSwaps_inverse)
    , ("prev . permute"          , mytest prop_prev_permute)
    , ("next (last permutation)" , mytest prop_next_last)
    , ("next . prev"             , mytest prop_next_prev)
    , ("prev . next"             , mytest prop_prev_next)
    , ("fst . sort"              , mytest prop_fst_sort)
    , ("snd . sort"              , mytest prop_snd_sort)
    , ("fst . sortBy"            , mytest prop_fst_sortBy)
    , ("snd . sortBy"            , mytest prop_snd_sortBy)
    , ("order"                   , mytest prop_order)
    , ("orderBy"                 , mytest prop_orderBy)
    , ("rank"                    , mytest prop_rank)
    , ("rankBy"                  , mytest prop_rankBy)
    , ("swapsPermute . swaps"    , mytest prop_swapsPermute_swaps)
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

