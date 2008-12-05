module ST (
    tests_STPermute
    ) where
    
import Data.Permute
import Data.Permute.ST

import Test.Permute()
import qualified Test.Permute as Test

import Driver

prop_dummy = True

tests_STPermute = 
    [ ("dummy", mytest prop_dummy)
    ]
