{-# LANGUAGE Rank2Types #-}
module STChoose (
    tests_STChoose,
    smoke_STChoose
    ) where
    
import Control.Monad
import Control.Monad.ST

import Data.Choose
import Data.Choose.ST


import Driver
import Debug.Trace
import Test.QuickCheck hiding ( choose )
import qualified Test.QuickCheck as QC
import Text.Printf
    
import Test.Choose()
import qualified Test.Choose as Test

newChoose_S n k = choose n k
prop_NewChoose (Index n k) = let n' = n-1 in
    newChoose n' k `equivalent` newChoose_S n' k

newListChoose_S n k is = listChoose n k is
prop_NewListChoose (ListChoose n k is) =
    newListChoose n k is `equivalent` newListChoose_S n k is

newCopyChoose_S c = (c, c)
prop_NewCopyChoose =
    implements
        (\c -> newCopyChoose c >>= unsafeFreeze)
        (\c -> newCopyChoose_S c)

copyChoose_S dst src = ((), src, src)
prop_CopyChoose =
    copyChoose `implements2` copyChoose_S

setFirst_S c = ((), choose (possible c) (size c))
prop_SetFirst =
    setFirst `implements` setFirst_S 

getElem_S c i = (c `at` i, c)
prop_GetElem (Index k i) =
    forAll arbitrary $ \(Nat n) ->
    implementsFor (n+k) k
        (\c -> getElem   c i)
        (\c -> getElem_S c i)

prop_UnsafeGetElem (Index k i) =
    forAll arbitrary $ \(Nat n) ->    
    implementsFor (n+k) k
        (\c -> unsafeGetElem c i)
        (\c -> getElem_S c i)

getPossible_S c = (possible c, c)
prop_GetPossible = getPossible `implements` getPossible_S

getSize_S c = (length (elems c), c)
prop_GetSize = getSize `implements` getSize_S
      
getElems_S c = (elems c, c)
prop_GetElems = getElems `implements` getElems_S

prop_IsValid_Strict = runST $ do
    c <- newChoose 10 3
    setElem c 0 1
    valid <- isValid c
    setElem c 0 0
    return $ valid == False

tests_STChoose = 
    [ ("newChoose"               , mytest prop_NewChoose)
    , ("newListChoose"           , mytest prop_NewListChoose)
    , ("newCopyChoose"           , mytest prop_NewCopyChoose)
    , ("setFirst"                , mytest prop_SetFirst)
    , ("getElem"                 , mytest prop_GetElem)
    , ("unsafeGetElem"           , mytest prop_UnsafeGetElem)
    , ("getPossible"             , mytest prop_GetPossible)
    , ("getSize"                 , mytest prop_GetSize)
    , ("getElems"                , mytest prop_GetElems)
    ]

smoke_STChoose =
    [ ("isValid is strict"             , mytest prop_IsValid_Strict)
    ]

------------------------------------------------------------------------
-- 
-- The specification language
--
    
abstract :: STChoose s -> ST s Choose
abstract = freeze

commutes :: (Eq a, Show a) =>
    STChoose s -> (STChoose s -> ST s a) ->
        (Choose -> (a,Choose)) -> ST s Bool
commutes c a f = do
    old <- abstract c
    r   <- a c
    new <- abstract c
    let s      = f old
        s'     = (r,new)
        passed = s == s'
        
    when (not passed) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show s'))
              return ()
              
    return passed

equivalent :: (forall s . ST s (STChoose s)) -> Choose -> Bool
equivalent c s = runST $ do
    c' <- (c >>= abstract)
    when (not $ c' == s) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show c'))
            return ()
    return (c' == s)
    
implements :: (Eq a, Show a) =>
    (forall s . STChoose s -> ST s a) ->
    (Choose -> (a,Choose)) -> 
        Property
a `implements` f =
    forAll arbitrary $ \(Nat n) ->
    forAll (QC.choose (0,n)) $ \k ->
        implementsFor n k a f

implementsFor :: (Eq a, Show a) =>
    Int -> Int ->
    (forall s . STChoose s -> ST s a) ->
    (Choose -> (a,Choose)) -> 
        Property
implementsFor n k a f =
    forAll (Test.choose n k) $ \c ->
        runST $ do
            c' <- unsafeThaw c
            commutes c' a f

implementsIf :: (Eq a, Show a) =>
    (forall s . STChoose s -> ST s Bool) ->
    (forall s . STChoose s -> ST s a) ->
    (Choose -> (a, Choose)) -> 
        Property
implementsIf pre a f =
    forAll arbitrary $ \c ->
        runST ( do
            c' <- thaw c
            pre c') ==>
        runST ( do
            c' <- unsafeThaw c
            commutes c' a f )

commutes2 :: (Eq a, Show a) =>
    STChoose s -> STChoose s -> (STChoose s -> STChoose s -> ST s a) ->
        (Choose -> Choose -> (a,Choose,Choose)) -> ST s Bool
commutes2 c1 c2 a f = do
    oldc1 <- abstract c1
    oldc2 <- abstract c2
    r   <- a c1 c2
    newc1 <- abstract c1
    newc2 <- abstract c2
    let s      = f oldc1 oldc2
        s'     = (r,newc1,newc2)
        passed = s == s'
        
    when (not passed) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show s'))
              return ()
              
    return passed

implements2 :: (Eq a, Show a) =>
    (forall s . STChoose s -> STChoose s -> ST s a) ->
    (Choose -> Choose -> (a,Choose,Choose)) -> 
        Property
implements2 a f =
    forAll arbitrary $ \(Nat n) ->
    forAll (QC.choose (0,n)) $ \k ->
    forAll (Test.choose n k) $ \c1 ->
    forAll (Test.choose n k) $ \c2 ->
        runST $ do
            c1' <- unsafeThaw c1
            c2' <- unsafeThaw c2
            commutes2 c1' c2' a f

