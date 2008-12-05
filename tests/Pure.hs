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


tests_Permute = 
    [ ("elems . permute", mytest prop_elems_permute)
    ]
