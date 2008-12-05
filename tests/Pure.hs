module Pure (
    tests_Permute
    ) where
    
import Data.Permute


import Driver
import Test.QuickCheck
    
import Test.Permute()
import qualified Test.Permute as Test


prop_elems_permute =
    forAll arbitrary $ \(Nat n) ->
        elems (permute n) == [0..(n-1)]

prop_apply =
    forAll arbitrary $ \(Index i n) ->
    forAll (Test.permute n) $ \p ->
        apply p i == (elems p) !! i

tests_Permute = 
    [ ("elems . permute", mytest prop_elems_permute)
    , ("apply"          , mytest prop_apply)
    ]
