{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.IOBase
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Permute.IOBase
    where

import Control.Monad
import Control.Monad.ST
import Data.Permute.Base

-- | A mutable permutation that can be manipulated in the 'IO' monad. The
-- type argument @s@ is the state variable argument for the 'IO' type.
newtype IOPermute = IOPermute (STPermute RealWorld)

newIOPermute :: Int -> IO (IOPermute)
newIOPermute n = 
    liftM IOPermute $ stToIO (newSTPermute n)
{-# INLINE newIOPermute #-}

newIOPermute_ :: Int -> IO (IOPermute)
newIOPermute_ n = 
    liftM IOPermute $ stToIO (newSTPermute_ n)
{-# INLINE newIOPermute_ #-}
        
newListIOPermute :: Int -> [Int] -> IO (IOPermute)
newListIOPermute n is = 
    liftM IOPermute $ stToIO (newListSTPermute n is)
{-# INLINE newListIOPermute #-}

unsafeNewListIOPermute :: Int -> [Int] -> IO (IOPermute)
unsafeNewListIOPermute n is = 
    liftM IOPermute $ stToIO (unsafeNewListSTPermute n is)
{-# INLINE unsafeNewListIOPermute #-}

newInvSwapsIOPermute :: Int -> [(Int,Int)] -> IO (IOPermute)
newInvSwapsIOPermute n ss = 
        liftM IOPermute $ stToIO (newInvSwapsSTPermute n ss)
{-# INLINE newInvSwapsIOPermute #-}

unsafeNewInvSwapsIOPermute :: Int -> [(Int,Int)] -> IO (IOPermute)
unsafeNewInvSwapsIOPermute n ss = 
    liftM IOPermute $ stToIO (unsafeNewInvSwapsSTPermute n ss)    
{-# INLINE unsafeNewInvSwapsIOPermute #-}

newCopyIOPermute :: IOPermute -> IO (IOPermute)
newCopyIOPermute (IOPermute p) =
    liftM IOPermute $ stToIO (newCopySTPermute p)
{-# INLINE newCopyIOPermute #-}

copyIOPermute :: IOPermute -> IOPermute -> IO ()
copyIOPermute (IOPermute dst) (IOPermute src) = stToIO $
    copySTPermute dst src
{-# INLINE copyIOPermute #-}

-- | Set a permutation to the identity.
setIdentityIOPermute :: IOPermute -> IO ()
setIdentityIOPermute (IOPermute p) = stToIO $ setIdentitySTPermute p
{-# INLINE setIdentityIOPermute #-}

getElemIOPermute :: IOPermute -> Int -> IO Int
getElemIOPermute (IOPermute p) i = stToIO $ getElemSTPermute p i
{-# INLINE getElemIOPermute #-}

unsafeGetElemIOPermute :: IOPermute -> Int -> IO Int
unsafeGetElemIOPermute (IOPermute p) i = stToIO $ unsafeGetElemSTPermute p i
{-# INLINE unsafeGetElemIOPermute #-}

swapElemsIOPermute :: IOPermute -> Int -> Int -> IO ()
swapElemsIOPermute (IOPermute p) i j = stToIO $
    swapElemsSTPermute p i j
{-# INLINE swapElemsIOPermute #-}

unsafeSwapElemsIOPermute :: IOPermute -> Int -> Int -> IO ()
unsafeSwapElemsIOPermute (IOPermute p) i j = stToIO $
    unsafeSwapElemsSTPermute p i j
{-# INLINE unsafeSwapElemsIOPermute #-}

getSizeIOPermute :: IOPermute -> IO Int
getSizeIOPermute (IOPermute p) = stToIO $ getSizeSTPermute p
{-# INLINE getSizeIOPermute #-}

sizeIOPermute :: IOPermute -> Int
sizeIOPermute (IOPermute p) = sizeSTPermute p
{-# INLINE sizeIOPermute #-}

getElemsIOPermute :: IOPermute -> IO [Int]
getElemsIOPermute (IOPermute p) = stToIO $ getElemsSTPermute p
{-# INLINE getElemsIOPermute #-}

isValidIOPermute :: IOPermute -> IO Bool
isValidIOPermute (IOPermute p) = stToIO $ isValidSTPermute p
{-# INLINE isValidIOPermute #-}

getInverseIOPermute :: IOPermute -> IO (IOPermute)
getInverseIOPermute (IOPermute p) =
    liftM IOPermute $ stToIO (getInverseSTPermute p)
{-# INLINE getInverseIOPermute #-}

copyInverseIOPermute :: IOPermute -> IOPermute -> IO ()
copyInverseIOPermute (IOPermute dst) (IOPermute src) = stToIO $ 
    copyInverseSTPermute dst src
{-# INLINE copyInverseIOPermute #-}
        
setNextIOPermute :: IOPermute -> IO Bool
setNextIOPermute (IOPermute p) = stToIO $ setNextSTPermute p
{-# INLINE setNextIOPermute #-}

setPrevIOPermute :: IOPermute -> IO Bool
setPrevIOPermute (IOPermute p) = stToIO $ setPrevSTPermute p
{-# INLINE setPrevIOPermute #-}

setNextByIOPermute :: (Int -> Int -> Ordering) -> IOPermute -> IO Bool
setNextByIOPermute cmp (IOPermute p) = stToIO $ setNextBySTPermute cmp p
{-# INLINE setNextByIOPermute #-}  
  
getSwapsIOPermute :: IOPermute -> IO [(Int,Int)]
getSwapsIOPermute (IOPermute p) = stToIO $ getSwapsSTPermute p
{-# INLINE getSwapsIOPermute #-}

getInvSwapsIOPermute :: IOPermute -> IO [(Int,Int)]
getInvSwapsIOPermute (IOPermute p) = stToIO $ getInvSwapsSTPermute p
{-# INLINE getInvSwapsIOPermute #-}

freezeIOPermute :: IOPermute -> IO Permute
freezeIOPermute (IOPermute p) = stToIO $ freezeSTPermute p
{-# INLINE freezeIOPermute #-}

unsafeFreezeIOPermute :: IOPermute -> IO Permute
unsafeFreezeIOPermute (IOPermute p) = stToIO $ unsafeFreezeSTPermute p
{-# INLINE unsafeFreezeIOPermute #-}

thawIOPermute :: Permute -> IO (IOPermute)
thawIOPermute p = liftM IOPermute $ stToIO (thawSTPermute p)
{-# INLINE thawIOPermute #-}

unsafeThawIOPermute :: Permute -> IO (IOPermute)
unsafeThawIOPermute p = liftM IOPermute $ stToIO (unsafeThawSTPermute p)
{-# INLINE unsafeThawIOPermute #-}
