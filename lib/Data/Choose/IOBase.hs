{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose.IOBase
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Choose.IOBase
    where

import Control.Monad
import Control.Monad.ST
import Data.Choose.Base

-- | A mutable combination that can be manipulated in the 'IO' monad.
newtype IOChoose = IOChoose (STChoose RealWorld) deriving Eq

newIOChoose :: Int -> Int -> IO (IOChoose)
newIOChoose n k = liftM IOChoose $ stToIO (newSTChoose n k)
{-# INLINE newIOChoose #-}

newIOChoose_ :: Int -> Int -> IO (IOChoose)
newIOChoose_ n k = liftM IOChoose $ stToIO (newSTChoose_ n k)
{-# INLINE newIOChoose_ #-}

getPossibleIOChoose :: IOChoose -> IO Int
getPossibleIOChoose (IOChoose c) = stToIO $ getPossibleSTChoose c
{-# INLINE getPossibleIOChoose #-}

possibleIOChoose :: IOChoose -> Int
possibleIOChoose (IOChoose c) = possibleSTChoose c
{-# INLINE possibleIOChoose #-}

getSizeIOChoose :: IOChoose -> IO Int
getSizeIOChoose (IOChoose c) = stToIO $ getSizeSTChoose c
{-# INLINE getSizeIOChoose #-}

sizeIOChoose :: IOChoose -> Int
sizeIOChoose (IOChoose c) = sizeSTChoose c
{-# INLINE sizeIOChoose #-}
        
unsafeGetElemIOChoose :: IOChoose -> Int -> IO Int
unsafeGetElemIOChoose (IOChoose c) i = stToIO $ unsafeGetElemSTChoose c i
{-# INLINE unsafeGetElemIOChoose #-}

unsafeSetElemIOChoose :: IOChoose -> Int -> Int -> IO ()
unsafeSetElemIOChoose (IOChoose c) i x = stToIO $ unsafeSetElemSTChoose c i x
{-# INLINE unsafeSetElemIOChoose #-}

getElemsIOChoose :: IOChoose -> IO [Int]
getElemsIOChoose (IOChoose c) = stToIO $ getElemsSTChoose c
{-# INLINE getElemsIOChoose #-}

setElemsIOChoose :: IOChoose -> [Int] -> IO ()
setElemsIOChoose (IOChoose c) is = stToIO $ setElemsSTChoose c is
{-# INLINE setElemsIOChoose #-}

unsafeFreezeIOChoose :: IOChoose -> IO Choose
unsafeFreezeIOChoose (IOChoose c) = stToIO $ unsafeFreezeSTChoose c
{-# INLINE unsafeFreezeIOChoose #-}

unsafeThawIOChoose :: Choose -> IO (IOChoose)
unsafeThawIOChoose c = liftM IOChoose $ stToIO (unsafeThawSTChoose c)
{-# INLINE unsafeThawIOChoose #-}
