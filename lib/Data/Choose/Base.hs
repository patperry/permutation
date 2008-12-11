-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose.Base
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Choose.Base
    where
        
import Control.Monad
import Control.Monad.ST
import Foreign

import Data.IntArray ( IntArray, STIntArray )
import qualified Data.IntArray as Arr
import qualified Data.IntArray as ArrST


--------------------------------- Choose ---------------------------------

-- | The immutable combination data type.  A way of representing @k@
-- unordered outcomes from @n@ possiblities.  The possibilites are
-- represented as the indices @0, ..., n-1@, and the outcomes are
-- given as a subset of size @k@.  The subset is stored with the indices
-- in ascending order.
data Choose = Choose {-# UNPACK #-} !Int      -- n
                     {-# UNPACK #-} !IntArray -- the subset of size k

unsafeAt :: Choose -> Int -> Int
unsafeAt (Choose _ arr) i = Arr.unsafeAt arr i
{-# INLINE unsafeAt #-}

-- | Get the number of outcomes, @k@.
size :: Choose -> Int
size (Choose _ arr) = Arr.numElements arr
{-# INLINE size #-}

-- | Get the number of possibilities, @n@.
possible :: Choose -> Int
possible (Choose n _) = n
{-# INLINE possible #-}

-- | Get a list of the @k@ outcomes.
elems :: Choose -> [Int]
elems (Choose _ arr) = Arr.elems arr
{-# INLINE elems #-}

instance Show Choose where
    show c = "listChoose " ++ show n ++ " " ++ show k ++ " " ++ show es
      where
        n  = possible c
        k  = size c
        es = elems c
    
instance Eq Choose where
    (==) c1 c2 = (   (possible c1 == possible c2)
                 &&      (size c1 == size c2)
                 &&     (elems c1 == elems c2)
                 )


--------------------------------- STChoose --------------------------------

-- | A mutable combination that can be manipulated in the 'ST' monad. The
-- type argument @s@ is the state variable argument for the 'ST' type.
data STChoose s = STChoose {-# UNPACK #-} !Int            -- n
                           {-# UNPACK #-} !(STIntArray s) -- the subset

getSizeSTChoose :: STChoose s -> ST s Int
getSizeSTChoose (STChoose _ marr) = ArrST.getNumElements marr
{-# INLINE getSizeSTChoose #-}

sizeSTChoose :: STChoose s -> Int
sizeSTChoose (STChoose _ marr) = ArrST.numElementsSTIntArray marr
{-# INLINE sizeSTChoose #-}

getPossibleSTChoose :: STChoose s -> ST s Int
getPossibleSTChoose (STChoose n _) = return n
{-# INLINE getPossibleSTChoose #-}

possibleSTChoose :: STChoose s -> Int
possibleSTChoose (STChoose n _) = n
{-# INLINE possibleSTChoose #-}

newSTChoose :: Int -> Int -> ST s (STChoose s)
newSTChoose n k = do
    c@(STChoose _ marr) <- newSTChoose_ n k
    ArrST.writeElems marr [0 .. k-1]
    return c
{-# INLINE newSTChoose #-}

newSTChoose_ :: Int -> Int -> ST s (STChoose s)
newSTChoose_ n k = do
    when (n < 0)          $ fail "invalid number of possibilities"
    when (k < 0 || k > n) $ fail "invalid outcome size"
    liftM (STChoose n) $ ArrST.newArray_ k
{-# INLINE newSTChoose_ #-}

unsafeGetElemSTChoose :: STChoose s -> Int -> ST s Int
unsafeGetElemSTChoose (STChoose _ marr) i = ArrST.unsafeRead marr i
{-# INLINE unsafeGetElemSTChoose #-}

unsafeSetElemSTChoose :: STChoose s -> Int -> Int -> ST s ()
unsafeSetElemSTChoose (STChoose _ marr) i x = ArrST.unsafeWrite marr i x
{-# INLINE unsafeSetElemSTChoose #-}

getElemsSTChoose :: STChoose s -> ST s [Int]
getElemsSTChoose (STChoose _ marr) = ArrST.readElems marr
{-# INLINE getElemsSTChoose #-}

setElemsSTChoose :: STChoose s -> [Int] -> ST s ()
setElemsSTChoose (STChoose _ marr) is = ArrST.writeElems marr is
{-# INLINE setElemsSTChoose #-}

unsafeFreezeSTChoose :: STChoose s -> ST s Choose
unsafeFreezeSTChoose (STChoose n marr) = 
    (liftM (Choose n) . ArrST.unsafeFreeze) marr
{-# INLINE unsafeFreezeSTChoose #-}

unsafeThawSTChoose :: Choose -> ST s (STChoose s)
unsafeThawSTChoose (Choose n arr) =
    (liftM (STChoose n) . ArrST.unsafeThaw) arr
{-# INLINE unsafeThawSTChoose #-}

instance Eq (STChoose s) where
    (==) (STChoose _ marr1) (STChoose _ marr2) = 
         ArrST.sameSTIntArray marr1 marr2
