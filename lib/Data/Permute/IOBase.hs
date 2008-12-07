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
newIOPermute n = liftM IOPermute $ stToIO (newSTPermute n)
{-# INLINE newIOPermute #-}

newIOPermute_ :: Int -> IO (IOPermute)
newIOPermute_ n = liftM IOPermute $ stToIO (newSTPermute_ n)
{-# INLINE newIOPermute_ #-}

getSizeIOPermute :: IOPermute -> IO Int
getSizeIOPermute (IOPermute p) = stToIO $ getSizeSTPermute p
{-# INLINE getSizeIOPermute #-}

sizeIOPermute :: IOPermute -> Int
sizeIOPermute (IOPermute p) = sizeSTPermute p
{-# INLINE sizeIOPermute #-}
        
unsafeGetElemIOPermute :: IOPermute -> Int -> IO Int
unsafeGetElemIOPermute (IOPermute p) i = stToIO $ unsafeGetElemSTPermute p i
{-# INLINE unsafeGetElemIOPermute #-}

unsafeSetElemIOPermute :: IOPermute -> Int -> Int -> IO ()
unsafeSetElemIOPermute (IOPermute p) i x = stToIO $ unsafeSetElemSTPermute p i x
{-# INLINE unsafeSetElemIOPermute #-}

unsafeSwapElemsIOPermute :: IOPermute -> Int -> Int -> IO ()
unsafeSwapElemsIOPermute (IOPermute p) i j = stToIO $
    unsafeSwapElemsSTPermute p i j
{-# INLINE unsafeSwapElemsIOPermute #-}

getElemsIOPermute :: IOPermute -> IO [Int]
getElemsIOPermute (IOPermute p) = stToIO $ getElemsSTPermute p
{-# INLINE getElemsIOPermute #-}

setElemsIOPermute :: IOPermute -> [Int] -> IO ()
setElemsIOPermute (IOPermute p) is = stToIO $ setElemsSTPermute p is
{-# INLINE setElemsIOPermute #-}

unsafeFreezeIOPermute :: IOPermute -> IO Permute
unsafeFreezeIOPermute (IOPermute p) = stToIO $ unsafeFreezeSTPermute p
{-# INLINE unsafeFreezeIOPermute #-}

unsafeThawIOPermute :: Permute -> IO (IOPermute)
unsafeThawIOPermute p = liftM IOPermute $ stToIO (unsafeThawSTPermute p)
{-# INLINE unsafeThawIOPermute #-}
