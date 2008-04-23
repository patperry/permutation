{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permutation
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Permutation (
    -- * The Permutation type
    Permutation,

    -- * Creating permutations
    permutation,
    identity,
    inverse,
    
    -- * Permutation properties
    size,
    apply,

    -- * Applying permutations
    applyWith,
    invertWith,

    -- * Converstion to/from other types
    -- ** @ForeignPtr@s
    fromForeignPtr,
    toForeignPtr,
    
    -- ** Lists
    toList,
    fromList,

    -- * Unsafe operations
    unsafePermutation,
    unsafeApply,
    
    ) where
        
import Control.Monad         ( foldM, liftM )
import Data.IntSet           ( IntSet )
import qualified Data.IntSet as IntSet
import Foreign               ( Ptr, ForeignPtr, mallocForeignPtrArray, 
                               withForeignPtr, pokeArray, peekArray, 
                               advancePtr, peek, peekElemOff, pokeElemOff ) 
import System.IO.Unsafe      ( unsafePerformIO )

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 ( realWorld# )
import GHC.IOBase               ( IO(IO) )
#endif

inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif
{-# INLINE inlinePerformIO #-}


-- | Represents a permutation of the integers @[0..n)@.        
data Permutation n =
        Perm !Int              -- size
             !(ForeignPtr Int) -- data

-- | Get the raw data array and size
toForeignPtr :: Permutation n -> (Int, ForeignPtr Int)
toForeignPtr (Perm n fptr) = (n, fptr)

-- | Convert and array and a size to a permutation.  No validation is
-- performed on the arguments.
fromForeignPtr :: Int -> ForeignPtr Int -> Permutation n
fromForeignPtr = Perm

-- | Gets the value of @n@.
size :: Permutation n -> Int
size (Perm n _) = n

-- | Apply a permutation to an integer.  The integer must be in the range
--   @[0..n)@.
apply :: Permutation n -> Int -> Int
apply p@(Perm n _) i 
    | i < 0 || i >= n =
        error $ 
            "applyPerm: Tried to apply permutation of size `" ++ show n ++
            "' to the value `" ++ show i ++ "'."
    | otherwise =
        unsafeApply p i

-- | Same as 'apply' but does not range-check the argument.
unsafeApply :: Permutation n -> Int -> Int
unsafeApply (Perm _ fptr) i =
    inlinePerformIO $ do
        withForeignPtr fptr $ flip peekElemOff i

-- | Create a permutation from a list of values.  The list must be of length
--   @n@ and contain each integer in @{0, 1, ..., (n-1) }@ exactly once.
--   The permutation that is returned will send the integer @i@ to its index
--   in the list.
permutation :: Int -> [Int] -> Permutation n
permutation n is = 
    let p = unsafePermutation n is
    in case isValid p of
           False -> error $ "Not a valid permutation."
           True  -> p

-- | Same as 'permutation', but does not check that the inputs are valid.
unsafePermutation :: Int -> [Int] -> Permutation n
unsafePermutation n is = 
    unsafePerformIO $ do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr -> pokeArray ptr is
        return $ Perm n fptr
{-# NOINLINE unsafePermutation #-}


fromList :: [Int] -> Permutation n
fromList is = permutation (length is) is

toList :: Permutation n -> [Int]
toList (Perm n fptr) =
    unsafePerformIO $ withForeignPtr fptr $ peekArray n

-- | Create an identity permutation of the given size.
identity :: Int -> Permutation n
identity n =
    unsafePerformIO $ do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr -> pokeArray ptr [0..(n-1)]
        return $ Perm n fptr


-- | Get the inverse of a permutation.
inverse :: Permutation n -> Permutation n
inverse p =
    let n = size p
    in
        unsafePerformIO $ do
            fptr <- mallocForeignPtrArray n
            withForeignPtr fptr $ \ptr -> do
                pokeArray ptr [0..(n-1)]
                invertWith (swap ptr) p
            return $ Perm n fptr
    where
        swap :: Ptr Int -> Int -> Int -> IO ()
        swap ptr i j = do
            x <- peekElemOff ptr i
            y <- peekElemOff ptr j
            pokeElemOff ptr i y
            pokeElemOff ptr j x
{-# NOINLINE inverse #-}




isValid :: Permutation n -> Bool
isValid (Perm n fptr) =
    unsafePerformIO $ 
        withForeignPtr fptr $ \ptr -> do
            liftM and $
                mapM (\i -> peekElemOff ptr i 
                            >>= \p -> isValidI ptr p i)
                     [0..(n-1)]
             
    where
        isValidI :: Ptr Int -> Int -> Int -> IO Bool
        isValidI ptr p i =
            liftM and $ 
                sequence [ inRange p, isUnique p ptr i ]
    
        inRange :: Int -> IO Bool
        inRange p =
            return $ p >= 0 && p < n
        
        isUnique :: Int -> Ptr Int -> Int -> IO Bool
        isUnique p ptr' n'
            | n' == 0 =
                return True
            | otherwise = do
                p' <- peek ptr'
                if p' == p 
                    then return False
                    else isUnique p (ptr' `advancePtr` 1) (n'-1)
    

-- | @invertWith swap p@ applies the inverse of the permutation as a 
-- sequence of swaps.  After this function is applied, @OUT[i] = IN[P[i]]@
invertWith :: (Monad m) => (Int -> Int -> m ()) -> Permutation n -> m ()
invertWith swap p =
    let n = size p
    in foldM (flip $ doCycle swap) IntSet.empty [0..(n-1)] >> return ()
    
    where
        doCycle :: (Monad m) => 
            (Int -> Int -> m ()) -> Int -> IntSet -> m (IntSet)
        doCycle swp i visited = 
            if i `IntSet.member` visited 
                then return visited
                else let visited' = IntSet.insert i visited
                         next     = unsafeApply p i
                     in doCycle' swp i i next visited'
            
        doCycle' :: (Monad m) => 
            (Int -> Int -> m ()) -> Int -> Int -> Int -> IntSet -> m (IntSet)
        doCycle' swp start cur next visited
            | next == start =
                return visited 
            | otherwise = 
                let visited' = IntSet.insert next visited
                    next'    = unsafeApply p next
                in do
                    swp cur next
                    doCycle' swp start next next' visited'

-- | @applyWith swap perm@ applies the permutation as a sequence of swaps.  After 
-- this function is applied, @OUT[P[i]] = IN[i]@
applyWith :: (Monad m) => (Int -> Int -> m ()) -> Permutation n -> m ()
applyWith swap p =
    let n = size p
    in foldM (flip $ doCycle swap) IntSet.empty [0..(n-1)] >> return ()
    
    where
        doCycle :: Monad m => 
            (Int -> Int -> m ()) -> Int -> IntSet -> m (IntSet)
        doCycle swp i visited = 
            if i `IntSet.member` visited 
                then return visited
                else let visited' = IntSet.insert i visited
                         cur      = unsafeApply p i
                     in doCycle' swp i cur visited'
            
        doCycle' :: Monad m => (Int -> Int -> m ()) -> Int -> Int -> IntSet -> m (IntSet)
        doCycle' swp start cur visited
            | cur == start =
                return visited 
            | otherwise = 
                let visited' = IntSet.insert cur visited
                    cur'     = unsafeApply p cur
                in do
                    swp start cur
                    doCycle' swp start cur' visited'
    