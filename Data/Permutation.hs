{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
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
    withPermutationPtr,
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
data Permutation =
        Perm {-# UNPACK #-} !Int             
             {-# UNPACK #-} !(ForeignPtr Int)
             {-# UNPACK #-} !Int

-- | Get the size, raw data array and offset
toForeignPtr :: Permutation -> (Int, ForeignPtr Int, Int)
toForeignPtr (Perm n f o) = (n, f, o)

-- | Convert size and raw array to a permutation.  No validation is
-- performed on the arguments.
fromForeignPtr :: Int -> ForeignPtr Int -> Int -> Permutation
fromForeignPtr = Perm

-- | Get the size of the permutation.
size :: Permutation -> Int
size (Perm n _ _) = n
{-# INLINE size #-}

-- | Perform an operation, given a pointer to the start of the
-- permutation data
withPermutationPtr :: Permutation -> (Ptr Int -> IO a) -> IO a
withPermutationPtr (Perm _ fptr off) f =
    withForeignPtr fptr $ \ptr ->
        f (ptr `advancePtr` off)

-- | Apply a permutation to an integer.  The integer must be in the range
--   @[0..n)@.
apply :: Permutation -> Int -> Int
apply p@(Perm n _ _) i 
    | i < 0 || i >= n =
        error $ 
            "applyPerm: Tried to apply permutation of size `" ++ show n ++
            "' to the value `" ++ show i ++ "'."
    | otherwise =
        unsafeApply p i
{-# INLINE apply #-}

-- | Same as 'apply' but does not range-check the argument.
unsafeApply :: Permutation -> Int -> Int
unsafeApply p i =
    inlinePerformIO $ do
        withPermutationPtr p $ flip peekElemOff i
{-# INLINE unsafeApply #-}

-- | Create a permutation from a list of values.  The list must be of length
--   @n@ and contain each integer in @{0, 1, ..., (n-1) }@ exactly once.
--   The permutation that is returned will send the integer @i@ to its index
--   in the list.
permutation :: Int -> [Int] -> Permutation
permutation n is = 
    let p = unsafePermutation n is
    in case isValid p of
           False -> error $ "Not a valid permutation."
           True  -> p

-- | Same as 'permutation', but does not check that the inputs are valid.
unsafePermutation :: Int -> [Int] -> Permutation
unsafePermutation n is = 
    unsafePerformIO $ do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr -> pokeArray ptr is
        return $ fromForeignPtr n fptr 0
{-# NOINLINE unsafePermutation #-}


fromList :: [Int] -> Permutation
fromList is = permutation (length is) is

toList :: Permutation -> [Int]
toList p = unsafePerformIO $ 
               withPermutationPtr p $ peekArray (size p)

-- | Create an identity permutation of the given size.
identity :: Int -> Permutation
identity n =
    unsafePerformIO $ do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr -> pokeArray ptr [0..(n-1)]
        return $ fromForeignPtr n fptr 0


-- | Get the inverse of a permutation.
inverse :: Permutation -> Permutation
inverse p =
    let n = size p
    in
        unsafePerformIO $ do
            fptr <- mallocForeignPtrArray n
            withForeignPtr fptr $ \ptr -> do
                pokeArray ptr [0..(n-1)]
                invertWith (swap ptr) p
            return $ fromForeignPtr n fptr 0
    where
        swap :: Ptr Int -> Int -> Int -> IO ()
        swap ptr i j = do
            x <- peekElemOff ptr i
            y <- peekElemOff ptr j
            pokeElemOff ptr i y
            pokeElemOff ptr j x
{-# NOINLINE inverse #-}




isValid :: Permutation -> Bool
isValid p@(Perm n _ _) =
    unsafePerformIO $
        withPermutationPtr p $ \ptr -> do
            liftM and $
                mapM (\i -> peekElemOff ptr i 
                            >>= \x -> isValidI ptr x i)
                     [0..(n-1)]
             
    where
        isValidI :: Ptr Int -> Int -> Int -> IO Bool
        isValidI ptr x i =
            liftM and $ 
                sequence [ inRange x, isUnique x ptr i ]
    
        inRange :: Int -> IO Bool
        inRange x =
            return $ x >= 0 && x < n
        
        isUnique :: Int -> Ptr Int -> Int -> IO Bool
        isUnique x ptr' n'
            | n' == 0 =
                return True
            | otherwise = do
                x' <- peek ptr'
                if x' == x 
                    then return False
                    else isUnique x (ptr' `advancePtr` 1) (n'-1)
    
-- | @applyWith swap perm@ applies the permutation as a sequence of swaps.  After 
-- this function is applied, @OUT[i] = IN[P[i]]@
applyWith :: (Monad m) => (Int -> Int -> m ()) -> Permutation -> m ()
applyWith swap p =
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

-- | @invertWith swap p@ applies the inverse of the permutation as a 
-- sequence of swaps.  After this function is applied, @OUT[P[i]] = IN[i]@
invertWith :: (Monad m) => (Int -> Int -> m ()) -> Permutation -> m ()
invertWith swap p =
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


instance Show Permutation where
    show p = "permutation "     ++ show (size p) ++ " " ++ show (toList p)
    
instance Eq Permutation where
    (==) p q = (size p == size q) && (toList p == toList q)
    