{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -XMagicHash -XUnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.Base
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Permute.Base 
    where

import Control.Monad
import Control.Monad.ST
import Foreign

import Data.IntArray ( IntArray, STIntArray )
import qualified Data.IntArray as Arr
import qualified Data.IntArray as ArrST


--------------------------------- Permute ---------------------------------

-- | The immutable permutation data type.
-- Internally, a permutation of size @n@ is stored as an
-- @0@-based array of @n@ 'Int's.  The permutation represents a reordering of
-- the integers @0, ..., (n-1)@.  The @i@th element of the array stores
-- the value @p[i]@. 
newtype Permute = Permute IntArray

unsafeApply :: Permute -> Int -> Int
unsafeApply (Permute p) i = Arr.unsafeAt p i
{-# INLINE unsafeApply #-}

-- | Get the size of the permutation.
size :: Permute -> Int
size (Permute p) = Arr.numElements p
{-# INLINE size #-}

-- | Get a list of the permutation elements.
elems :: Permute -> [Int]
elems (Permute p) = Arr.elems p
{-# INLINE elems #-}

instance Show Permute where
    show p = "listPermute " ++ show (size p) ++ " " ++ show (elems p)
    
instance Eq Permute where
    (==) p q = (size p == size q) && (elems p == elems q)


--------------------------------- STPermute --------------------------------

-- | A mutable permutation that can be manipulated in the 'ST' monad. The
-- type argument @s@ is the state variable argument for the 'ST' type.
newtype STPermute s = STPermute (STIntArray s)

getSizeSTPermute :: STPermute s -> ST s Int
getSizeSTPermute (STPermute marr) = ArrST.getNumElements marr
{-# INLINE getSizeSTPermute #-}

sizeSTPermute :: STPermute s -> Int
sizeSTPermute (STPermute marr) = ArrST.numElementsSTIntArray marr
{-# INLINE sizeSTPermute #-}

newSTPermute :: Int -> ST s (STPermute s)
newSTPermute n = do
    p@(STPermute marr) <- newSTPermute_ n
    ArrST.writeElems marr [0 .. n-1]
    return $! p
{-# INLINE newSTPermute #-}

newSTPermute_ :: Int -> ST s (STPermute s)
newSTPermute_ n = do
    when (n < 0) $ fail "invalid size"
    liftM STPermute $ ArrST.newArray_ n
{-# INLINE newSTPermute_ #-}

unsafeGetElemSTPermute :: STPermute s -> Int -> ST s Int
unsafeGetElemSTPermute (STPermute marr) i = ArrST.unsafeRead marr i
{-# INLINE unsafeGetElemSTPermute #-}

unsafeSetElemSTPermute :: STPermute s -> Int -> Int -> ST s ()
unsafeSetElemSTPermute (STPermute marr) i x = ArrST.unsafeWrite marr i x
{-# INLINE unsafeSetElemSTPermute #-}

unsafeSwapElemsSTPermute :: STPermute s -> Int -> Int -> ST s ()
unsafeSwapElemsSTPermute (STPermute marr) i j = ArrST.unsafeSwap marr i j
{-# INLINE unsafeSwapElemsSTPermute #-}

getElemsSTPermute :: STPermute s -> ST s [Int]
getElemsSTPermute (STPermute marr) = ArrST.readElems marr
{-# INLINE getElemsSTPermute #-}

setElemsSTPermute :: STPermute s -> [Int] -> ST s ()
setElemsSTPermute (STPermute marr) is = ArrST.writeElems marr is
{-# INLINE setElemsSTPermute #-}

unsafeFreezeSTPermute :: STPermute s -> ST s Permute
unsafeFreezeSTPermute (STPermute marr) = 
    (liftM Permute . ArrST.unsafeFreeze) marr
{-# INLINE unsafeFreezeSTPermute #-}

unsafeThawSTPermute :: Permute -> ST s (STPermute s)
unsafeThawSTPermute (Permute arr) =
    (liftM STPermute . ArrST.unsafeThaw) arr
{-# INLINE unsafeThawSTPermute #-}
