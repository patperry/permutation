module Pure (
    tests_Permute
    ) where
    
import Data.Permute

import Test.Permute()
import qualified Test.Permute as Test

import Driver

prop_dummy = True

tests_Permute = 
    [ ("dummy", mytest prop_dummy)
    ]
