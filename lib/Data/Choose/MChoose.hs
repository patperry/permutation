{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
        FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Choose.MChoose
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- An overloaded interface to mutable combinations. For combination types which 
-- can be used with this interface, see "Data.Choose.IO" and "Data.Choose.ST".
--

module Data.Choose.MChoose (
    -- * Class of mutable combination types
    MChoose, 

    -- * Constructing mutable combinations
    newChoose,
    newChoose_,
    newListChoose,
    newCopyChoose,
    copyChoose,
    setFirst,
    
    -- * Accessing combination elements
    getElem,
    setElem,
    
    -- * Combination properties
    getPossible,
    getSize,
    getElems,
    setElems,
    isValid,

    -- * Combination functions
    getComplement,
    getComplElems,
    setNext,
    setPrev,
    
    -- * Converstions between mutable and immutable combinations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
    
    -- * Unsafe operations
    unsafeNewListChoose,
    unsafeGetElem,
    unsafeSetElem,

    ) where

import Control.Monad
import Control.Monad.ST
import System.IO.Unsafe( unsafeInterleaveIO )

import Data.Choose.Base
import Data.Choose.IOBase


--------------------------------- MChoose --------------------------------

-- | Class for representing a mutable combination.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable combination will be
-- manipulated.
class (Monad m) => MChoose c m | c -> m, m -> c where
    -- | Get the number of possibilities, @n@ in the combination.
    getPossible :: c -> m Int
    
    -- | Get the number of outcomes, @k@ in the combination.
    getSize :: c -> m Int

    -- | @newChoose n k@ creates a new combination of @k@ outcomes from @n@ 
    -- possibilites initialized to the subset @{ 0, ..., k-1 }@.
    newChoose :: Int -> Int -> m c

    -- | @newChoose n k@ allocates a new combination of @k@ outcomes from
    -- @n@ possibilities but does not initialize it.
    newChoose_ :: Int -> Int -> m c

    unsafeGetElem :: c -> Int -> m Int
    unsafeSetElem :: c -> Int -> Int -> m ()

    -- | Get a lazy list of the combination elements.  The laziness makes this
    -- function slightly dangerous if you are modifying the combination.
    getElems :: c -> m [Int]

    -- | Set all the values of a combination from a list of elements.
    setElems :: c -> [Int] -> m ()

    unsafeFreeze :: c -> m Choose
    unsafeThaw :: Choose -> m c
    
    unsafeInterleaveM :: m a -> m a
    
        
-- | Construct a combination from a list of elements.  
-- @newListChoose n k is@ creates a combination of @k@ outcomes from @n@
-- possibilities initialized to have the @i@th element equal to @is !! i@.  
-- For the combination to be valid, the elements must all be unique, they 
-- must be in sorted order, and they all must be in the range @0 .. n-1@.
newListChoose :: (MChoose c m) => Int -> Int -> [Int] -> m c
newListChoose n k is = do
    c <- unsafeNewListChoose n k is
    valid <- isValid c
    when (not valid) $ fail "invalid combination"
    return c
{-# INLINE newListChoose #-}

unsafeNewListChoose :: (MChoose c m) => Int -> Int -> [Int] -> m c
unsafeNewListChoose n k is = do
    c <- newChoose_ n k
    setElems c is
    return c
{-# INLINE unsafeNewListChoose #-}

-- | Construct a new combination by copying another.
newCopyChoose :: (MChoose c m) => c -> m c
newCopyChoose c = do
    n  <- getPossible c
    k  <- getSize c
    c' <- newChoose_ n k
    copyChoose c' c
    return c'
{-# INLINE newCopyChoose #-}

-- | @copyChoose dst src@ copies the elements of the combination @src@
-- into the combination @dst@.  The two combinations must have the same
-- size.
copyChoose :: (MChoose c m) => c -> c -> m ()
copyChoose dst src =
    getElems src >>= setElems dst
{-# INLINE copyChoose #-}

-- | Set a combination to be the first subset of its size.
setFirst :: (MChoose c m) => c -> m ()
setFirst c = do
    k <- getSize c
    setElems c [0 .. k-1]
{-# INLINE setFirst #-}

-- | @getElem c i@ gets the value of the @i@th element of the combination
-- @c@.  The index @i@ must be in the range @0..k-1@, where @n@ is the
-- size of the combination.
getElem :: (MChoose c m) => c -> Int -> m Int
getElem c i = do
    k <- getSize c
    when (i < 0 || i >= k) $ fail "getElem: invalid index"
    unsafeGetElem c i
{-# INLINE getElem #-}

-- | @setElem c i x@ sets the value of the @i@th element of the combination
-- @c@.  The index @i@ must be in the range @0..k-1@, where @k@ is the
-- size of the combination.
setElem :: (MChoose c m) => c -> Int -> Int -> m ()
setElem c i x = do
    k <- getSize c
    when (i < 0 || i >= k) $ fail "getElem: invalid index"
    unsafeSetElem c i x
{-# INLINE setElem #-}

-- | Returns whether or not the combination is valid.  For it to be valid,
-- the elements must all be unique, they must be in sorted order, and they
-- all must be in the range @0 .. n-1@, where @n@ is the number of 
-- possibilies in the combination.
isValid :: (MChoose c m) => c -> m Bool
isValid c = do
    n  <- getPossible c
    is <- getElems c
    return $! go n (-1) is
  where
    go _ _ []     = True
    go n j (i:is) = i > j && i < n && go n i is
{-# INLINE isValid #-}

-- | Compute the complement of a combination
getComplement :: (MChoose c m) => c -> m c
getComplement c = do
    n <- getPossible c
    k <- getSize c
    d <- newChoose_ n (n-k)
    setElems d =<< getComplElems c
    return d
{-# INLINE getComplement #-}

-- | Return a lazy list of the elements in the complement of a combination.
-- If the combination is a subset of @k@ outcomes from @n@ possibilities, then
-- the returned list will be sorted and of length @n-k@.  
-- Due to the laziness, you should be careful when using this function if you
-- are also modifying the combination.
getComplElems :: (MChoose c m) => c -> m [Int]
getComplElems c = do
    n  <- getPossible c
    is <- getElems c
    return $ go n is 0
  where
    go n _      j | j == n    = []
    go n []     j             = [j .. n-1]
    go n (i:is) j | j == i    = go n is (j+1)
                  | otherwise = [j .. i-1] ++ go n is (i+1)
{-# INLINE getComplElems #-}

-- | Advance a combination to the next in lexicogrphic order and return @True@. 
--  If no further combinations are available, return @False@ and leave the 
-- combination unmodified.  Starting with @[ 0 .. k-1 ]@ and repeatedly
-- calling @setNext@ will iterate through all subsets of size @k@.
setNext :: (MChoose c m) => c -> m Bool
setNext c = do
    n <- getPossible c
    k <- getSize c
    if k > 0
        then do
            findIncrement (k-1) (n-1) >>=
                maybe (return False) (\(i,i') -> do
                    unsafeSetElem c i (i'+1)
                    setAscending k (i+1) (i'+2)
                    return True
                )
        else 
            return False
  where
    findIncrement i m = do
        i' <- unsafeGetElem c i
        if i' /= m then return (Just (i,i')) else recurse
      where
        recurse = if i /= 0 then findIncrement (i-1) (m-1) else return Nothing 

    setAscending k i x | i == k = return ()
                       | otherwise = do
        unsafeSetElem c i x
        setAscending k (i+1) (x+1)
{-# INLINE setNext #-}
        
-- | Step backwards to the previous combination in lexicographic order and
-- return @True@.  If there is no previous combination, return @False@ and
-- leave the combination unmodified.
setPrev :: (MChoose c m) => c -> m Bool
setPrev c = do
    n <- getPossible c
    k <- getSize c
    if k > 0
        then do
            k1' <- unsafeGetElem c (k-1)
            findGap (k-1) k1' >>=
                maybe (return False) (\(i,i') -> do
                    unsafeSetElem c i (i'-1)
                    setAscending k (i+1) (n-k+i+1)
                    return True
                )
        else
            return False
  where
    findGap i i' 
        | i == 0 = 
            if i' == 0 
                then return $ Nothing
                else return $ Just (0,i') 

        | otherwise = let j = i-1 in do
            j' <- unsafeGetElem c j
            if i' /= j'+1 
                then return $ Just (i,i')
                else findGap j j' 


    setAscending k i x | i == k = return ()
                       | otherwise = do
        unsafeSetElem c i x
        setAscending k (i+1) (x+1)
{-# INLINE setPrev #-}

-- | Convert a mutable combination to an immutable one.
freeze :: (MChoose c m) => c -> m Choose
freeze c = unsafeFreeze =<< newCopyChoose c
{-# INLINE freeze #-}

-- | Convert an immutable combination to a mutable one.
thaw :: (MChoose c m) => Choose -> m c
thaw c = newCopyChoose =<< unsafeThaw c
{-# INLINE thaw #-}


--------------------------------- Instances ---------------------------------

instance MChoose (STChoose s) (ST s) where
    getPossible = getPossibleSTChoose
    {-# INLINE getPossible #-}
    getSize = getSizeSTChoose
    {-# INLINE getSize #-}
    newChoose = newSTChoose
    {-# INLINE newChoose #-}
    newChoose_ = newSTChoose_
    {-# INLINE newChoose_ #-}
    unsafeGetElem = unsafeGetElemSTChoose
    {-# INLINE unsafeGetElem #-}
    unsafeSetElem = unsafeSetElemSTChoose
    {-# INLINE unsafeSetElem #-}
    getElems = getElemsSTChoose
    {-# INLINE getElems #-}
    setElems = setElemsSTChoose
    {-# INLINE setElems #-}
    unsafeFreeze = unsafeFreezeSTChoose
    {-# INLINE unsafeFreeze #-}
    unsafeThaw = unsafeThawSTChoose
    {-# INLINE unsafeThaw #-}
    unsafeInterleaveM = unsafeInterleaveST
    {-# INLINE unsafeInterleaveM #-}

instance MChoose IOChoose IO where
    getPossible = getPossibleIOChoose
    {-# INLINE getPossible #-}
    getSize = getSizeIOChoose
    {-# INLINE getSize #-}
    newChoose = newIOChoose
    {-# INLINE newChoose #-}
    newChoose_ = newIOChoose_
    {-# INLINE newChoose_ #-}
    unsafeGetElem = unsafeGetElemIOChoose
    {-# INLINE unsafeGetElem #-}
    unsafeSetElem = unsafeSetElemIOChoose
    {-# INLINE unsafeSetElem #-}
    getElems = getElemsIOChoose
    {-# INLINE getElems #-}
    setElems = setElemsIOChoose
    {-# INLINE setElems #-}
    unsafeFreeze = unsafeFreezeIOChoose
    {-# INLINE unsafeFreeze #-}
    unsafeThaw = unsafeThawIOChoose
    {-# INLINE unsafeThaw #-}
    unsafeInterleaveM = unsafeInterleaveIO
    {-# INLINE unsafeInterleaveM #-}
