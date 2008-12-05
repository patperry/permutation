{-# LANGUAGE Rank2Types #-}
module ST (
    tests_STPermute
    ) where
    
import Control.Monad
import Control.Monad.ST

import Data.Permute
import Data.Permute.ST


import Driver
import Debug.Trace
import Test.QuickCheck
import Text.Printf
    
import Test.Permute()
import qualified Test.Permute as Test



prop_dummy = True


tests_STPermute = 
    [ ("dummy", mytest prop_dummy)
    ]

------------------------------------------------------------------------
-- 
-- The specification language
--
    
abstract :: STPermute s -> ST s Permute
abstract = freeze

commutes :: (Eq a, Show a) =>
    STPermute s -> (STPermute s -> ST s a) ->
        (Permute -> (a,Permute)) -> ST s Bool
commutes p a f = do
    old <- abstract p
    r   <- a p
    new <- abstract p
    let s      = f old
        s'     = (r,new)
        passed = s == s'
        
    when (not passed) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show s'))
              return ()
              
    return passed

equivalent :: (forall s . ST s (STPermute s)) -> Permute -> Bool
equivalent p s = runST $ do
    p' <- (p >>= abstract)
    when (not $ p' == s) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show p'))
            return ()
    return (p' == s)
    
implements :: (Eq a, Show a) =>
    (forall s . STPermute s -> ST s a) ->
    (Permute -> (a,Permute)) -> 
        Property
a `implements` f =
    forAll arbitrary $ \(Nat n) ->
        implementsFor n a f

implementsFor :: (Eq a, Show a) =>
    Int ->
    (forall s . STPermute s -> ST s a) ->
    (Permute -> (a,Permute)) -> 
        Property
implementsFor n a f =
    forAll (Test.permute n) $ \p ->
        runST $ do
            commutes (unsafeThaw p) a f

implementsIf :: (Eq a, Show a) =>
    (forall s . STPermute s -> ST s Bool) ->
    (forall s . STPermute s -> ST s a) ->
    (Permute -> (a, Permute)) -> 
        Property
implementsIf pre a f =
    forAll arbitrary $ \p ->
        runST ( do
            p' <- thaw p
            pre p') ==>
        runST ( do
            commutes (unsafeThaw p) a f )
