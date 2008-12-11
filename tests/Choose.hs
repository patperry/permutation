{-# OPTIONS_GHC -fglasgow-exts #-}
module Choose (
    tests_Choose
    ) where
    
import Control.Monad.ST
import Data.Array.ST
import Data.List( foldl' )
import qualified Data.List as List
import Data.Maybe( fromJust )

import Data.Choose

import Driver
import Test.QuickCheck hiding (choose)
    
import Test.Choose()
import qualified Test.Choose as Test


prop_possible_choose (Index n' k) = let n = n'-1 in
    possible (choose n k) == n
prop_size_choose (Index n' k) = let n = n'-1 in
    size (choose n k) == k
prop_elems_choose (Index n' k) = let n = n'-1 in
    elems (choose n k) == [0 .. k-1]

prop_possible_listChoose (ListChoose n k is) =
    possible (listChoose n k is) == n
prop_size_listChoose (ListChoose n k is) =
    size (listChoose n k is) == k
prop_elems_listChoose (ListChoose n k is) =
    elems (listChoose n k is) == is

prop_at       = prop_at_help at
prop_unsafeAt = prop_at_help unsafeAt
prop_at_help a =
    forAll arbitrary $ \(Index k i) ->
    forAll arbitrary $ \(Nat nk) ->
    forAll (Test.choose (nk+k) k) $ \c ->
        a c i == (elems c) !! i

prop_possible_complement (c :: Choose) =
    possible (complement c) == possible c
prop_size_complement (c :: Choose) =
    size (complement c) == possible c - size c
prop_elems_complement (c :: Choose) =
    all (not . (`elem` is)) is'
  where
    is  = elems c
    is' = elems (complement c)

prop_prev_choose (Index n1 k) = let n = n1-1 in
    prev (choose n k) == Nothing
prop_next_last (Index n1 k) = let n = n1-1 in
    next (listChoose n k $ [(n-k)..(n-1)]) == Nothing

prop_next_prev (c :: Choose) =
    case prev c of
        Just c' -> c == (fromJust $ next c')
        Nothing -> c == choose n k
  where
    n = possible c
    k = size c
    
prop_prev_next (c :: Choose) =
    case next c of
        Just c' -> c == (fromJust $ prev c')
        Nothing -> c == (listChoose n k $ [(n-k)..(n-1)])
  where
    n = possible c
    k = size c

tests_Choose = 
    [ ("possible . choose"      , mytest prop_possible_choose)
    , ("size . choose"          , mytest prop_size_choose)
    , ("elems . choose"         , mytest prop_elems_choose)
    , ("possible . listChoose"  , mytest prop_possible_listChoose)
    , ("size . listChoose"      , mytest prop_size_listChoose)
    , ("elems . listChoose"     , mytest prop_elems_listChoose)
    , ("at"                     , mytest prop_at)
    , ("unsafeAt"               , mytest prop_unsafeAt)
    , ("possible . complement"  , mytest prop_possible_complement)
    , ("size . complement"      , mytest prop_size_complement)
    , ("elems . complement"     , mytest prop_elems_complement)
    , ("prev . choose"          , mytest prop_prev_choose)
    , ("next (last choose)"     , mytest prop_next_last)
    , ("next . prev"            , mytest prop_next_prev)
    , ("prev . next"            , mytest prop_prev_next)
    ]
