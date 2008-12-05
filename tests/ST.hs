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

newPermute_S n = permute n
prop_NewPermute (Nat n) = 
    newPermute n `equivalent` newPermute_S n

newListPermute_S n is = listPermute n is
prop_NewListPermute (ListPermute n is) =
    newListPermute n is `equivalent` newListPermute_S n is

newInvSwapsPermute_S n ss = invSwapsPermute n ss
prop_NewInvSwapsPermute (InvSwapsPermute n ss) =
    newInvSwapsPermute n ss `equivalent` newInvSwapsPermute_S n ss
prop_UnsafeNewInvSwapsPermute (InvSwapsPermute n ss) =
    unsafeNewInvSwapsPermute n ss `equivalent` newInvSwapsPermute_S n ss



newCopyPermute_S p = (p, p)
prop_NewCopyPermute =
    implements
        (\p -> newCopyPermute p >>= unsafeFreeze)
        (\p -> newCopyPermute_S p)

copyPermute_S p q = ((), q, q)
prop_CopyPermute =
    copyPermute `implements2` copyPermute_S

setIdentity_S p = ((), permute (size p))
prop_SetIdentity =
    setIdentity `implements` setIdentity_S 

getElem_S p i = ((elems p) !! i, p)
prop_GetElem (Index n i) =
    implementsFor n
        (\p -> getElem   p i)
        (\p -> getElem_S p i)

prop_UnsafeGetElem (Index n i) =
    implementsFor n
        (\p -> unsafeGetElem p i)
        (\p -> getElem_S p i)

swapElems_S p i j = ((), p')
  where
    (n,is) = (size p, elems p)
      
    at k | k == i    = is !! j
         | k == j    = is !! i
         | otherwise = is !! k
    p'   = listPermute n $ map at [0..(n-1)]

prop_SwapElems (Swap n i j) =
    implementsFor n
        (\p -> swapElems p i j) 
        (\p -> swapElems_S p i j)

prop_UnsafeSwapElems (Swap n i j) =
    implementsFor n
        (\p -> unsafeSwapElems p i j) 
        (\p -> swapElems_S p i j)


getSize_S p = (length (elems p), p)
prop_GetSize = getSize `implements` getSize_S
      
getElems_S p = (elems p, p)
prop_GetElems = getElems `implements` getElems_S




tests_STPermute = 
    [ ("newPermute"               , mytest prop_NewPermute)
    , ("newListPermute"           , mytest prop_NewListPermute)
    , ("newInvSwapsPermute"       , mytest prop_NewInvSwapsPermute)
    , ("unsafeNewInvSwapsPermute" , mytest prop_UnsafeNewInvSwapsPermute)
    , ("newCopyPermute"           , mytest prop_NewCopyPermute)
    , ("copyPermute"              , mytest prop_CopyPermute)
    , ("setIdentity"              , mytest prop_SetIdentity)
    , ("getElem"                  , mytest prop_GetElem)
    , ("unsafeGetElem"            , mytest prop_UnsafeGetElem)
    , ("swapElems"                , mytest prop_SwapElems)
    , ("unsafeSwapElems"          , mytest prop_UnsafeSwapElems)
    , ("getSize"                  , mytest prop_GetSize)
    , ("getElems"                 , mytest prop_GetElems)
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
            p' <- unsafeThaw p
            commutes p' a f

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
            p' <- unsafeThaw p
            commutes p' a f )


commutes2 :: (Eq a, Show a) =>
    STPermute s -> STPermute s -> (STPermute s -> STPermute s -> ST s a) ->
        (Permute -> Permute -> (a,Permute,Permute)) -> ST s Bool
commutes2 p q a f = do
    oldp <- abstract p
    oldq <- abstract q
    r   <- a p q
    newp <- abstract p
    newq <- abstract q
    let s      = f oldp oldq
        s'     = (r,newp,newq)
        passed = s == s'
        
    when (not passed) $
        trace (printf ("expected `%s' but got `%s'") (show s) (show s'))
              return ()
              
    return passed


implements2 :: (Eq a, Show a) =>
    (forall s . STPermute s -> STPermute s -> ST s a) ->
    (Permute -> Permute -> (a,Permute,Permute)) -> 
        Property
implements2 a f =
    forAll arbitrary $ \(Nat n) ->
    forAll (Test.permute n) $ \p ->
    forAll (Test.permute n) $ \q ->
        runST $ do
            p' <- unsafeThaw p
            q' <- unsafeThaw q
            commutes2 p' q' a f

