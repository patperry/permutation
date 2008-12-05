{-# LANGUAGE ScopedTypeVariables #-}
module Pure (
    tests_Permute
    ) where
    
import Data.Permute


import Driver
import Test.QuickCheck
    
import Test.Permute()
import qualified Test.Permute as Test


prop_size_permute (Nat n) =
    size (permute n) == n
prop_elems_permute (Nat n) =
    elems (permute n) == [0..(n-1)]


prop_size_listPermute (ListPerm n is) =
    size (listPermute n is) == n
prop_elems_listPermute (ListPerm n is) =
    elems (listPermute n is) == is


prop_apply       = prop_apply_help apply
prop_unsafeApply = prop_apply_help unsafeApply
prop_apply_help a =
    forAll arbitrary $ \(Index i n) ->
    forAll (Test.permute n) $ \p ->
        a p i == (elems p) !! i


tests_Permute = 
    [ ("size . permute"        , mytest prop_size_permute)
    , ("elems . permute"       , mytest prop_elems_permute)
    , ("size . listPermute"    , mytest prop_size_listPermute)
    , ("elems . listPermute"   , mytest prop_elems_listPermute)
    , ("apply"                 , mytest prop_apply)
    , ("unsafeApply"           , mytest prop_unsafeApply)
    ]
