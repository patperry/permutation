-----------------------------------------------------------------------------
-- |
-- Module     : System.Random.Permutation
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module System.Random.Permutation (
    permutation,
    shuffleWith,
    ) where

import Control.Monad.Trans ( lift )
import Foreign             ( ForeignPtr, mallocForeignPtrArray, withForeignPtr,
                             unsafeForeignPtrToPtr, touchForeignPtr,
                             pokeArray, advancePtr, peek, poke )
import System.IO.Unsafe    ( unsafePerformIO )

import Data.Permutation    ( Permutation )
import qualified Data.Permutation as P

import System.Random.Gen

-- | Get a random permutation of the given size.
permutation :: Monad m => Int -> GenT m (Permutation n)
permutation = shuffleWith (\_ _ -> return ())

-- | @shuffleWith swap n g@  Applies random swaps to pairs of integers in the
--   range @[0..n)@, such that a uniform permutation is generated.  The 
--   inverse of the permutation is returned.
shuffleWith :: Monad m => (Int -> Int -> m ()) -> Int -> GenT m (Permutation n)
shuffleWith swap n =
    let fptr = unsafePerformIO $ do
                   f <- mallocForeignPtrArray n
                   withForeignPtr f $ flip pokeArray [0..(n-1)]
                   return f
    in do
        mapM_ (step $ swap' swap fptr) [0..(n-2)]
        return $ P.fromForeignPtr n fptr
        
    where
    
    step :: Monad m => (Int -> Int -> m ()) -> Int -> GenT m ()
    step swp i = do
        i' <- unifInt (n-i) >>= return . (i+)
        if i == i'
            then return ()
            else lift (swp i i')

    swap' :: (Monad m) => (Int -> Int -> m ()) -> ForeignPtr Int -> Int -> Int -> m ()
    swap' f fptr i j = 
        let ptr  = unsafeForeignPtrToPtr fptr
            ptrI = ptr `advancePtr` i
            ptrJ = ptr `advancePtr` j
            io   = unsafePerformIO $ do
                       p <- peek ptrI
                       q <- peek ptrJ
                       poke ptrI q
                       poke ptrJ p
                       touchForeignPtr fptr
            in io `seq` (f i j)
{-# NOINLINE shuffleWith #-}
