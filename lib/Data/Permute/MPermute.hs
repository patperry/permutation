{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
        FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.MPermute
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- An overloaded interface to mutable permutations. For permutation types which 
-- can be used with this interface, see "Data.Permute.IO" and "Data.Permute.ST".
--

module Data.Permute.MPermute (
    -- * Class of mutable permutation types
    MPermute, 

    -- * Constructing mutable permutations
    newPermute,
    newPermute_,
    newListPermute,
    newSwapsPermute,
    newCopyPermute,
    copyPermute,
    setIdentity,
    
    -- * Accessing permutation elements
    getElem,
    setElem,
    swapElems,
    
    -- * Permutation properties
    getSize,
    getElems,
    setElems,
    isValid,

    -- * Permutation functions
    getInverse,
    copyInverse,
    setNext,
    setPrev,
    
    -- * Applying permutations
    getSwaps,
    getInvSwaps,
    
    -- * Sorting
    getSort,
    getSortBy,
    getOrder,
    getOrderBy,
    getRank,
    getRankBy,
    
    -- * Converstions between mutable and immutable permutations
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
    
    -- * Unsafe operations
    unsafeNewListPermute,
    unsafeNewSwapsPermute,
    unsafeGetElem,
    unsafeSetElem,
    unsafeSwapElems,

    ) where

import Control.Monad
import Control.Monad.ST
import Data.Function( on )
import qualified Data.List as List
import System.IO.Unsafe( unsafeInterleaveIO )

import Data.Permute.Base
import Data.Permute.IOBase


--------------------------------- MPermute --------------------------------

-- | Class for representing a mutable permutation.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable permutation will be
-- manipulated.
class (Monad m) => MPermute p m | p -> m, m -> p where
    -- | Get the size of a permutation.
    getSize :: p -> m Int

    -- | Create a new permutation initialized to be the identity.
    newPermute :: Int -> m p

    -- | Allocate a new permutation but do not initialize it.
    newPermute_ :: Int -> m p

    unsafeGetElem :: p -> Int -> m Int
    unsafeSetElem :: p -> Int -> Int -> m ()
    unsafeSwapElems :: p -> Int -> Int -> m ()    

    -- | Get a lazy list of the permutation elements.  The laziness makes this
    -- function slightly dangerous if you are modifying the permutation.
    getElems :: p -> m [Int]

    -- | Set all the values of a permutation from a list of elements.
    setElems :: p -> [Int] -> m ()

    unsafeFreeze :: p -> m Permute
    unsafeThaw :: Permute -> m p
    
    unsafeInterleaveM :: m a -> m a
    
        
-- | Construct a permutation from a list of elements.  
-- @newListPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
newListPermute :: (MPermute p m) => Int -> [Int] -> m p
newListPermute n is = do
    p <- unsafeNewListPermute n is
    valid <- isValid p
    when (not valid) $ fail "invalid permutation"
    return $! p
{-# INLINE newListPermute #-}

unsafeNewListPermute :: (MPermute p m) => Int -> [Int] -> m p
unsafeNewListPermute n is = do
    p <- newPermute_ n
    setElems p is
    return $! p
{-# INLINE unsafeNewListPermute #-}

-- | Construct a permutation from a list of swaps.
-- @newSwapsPermute n ss@ creates a permuation of size @n@ given a
-- sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
newSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
newSwapsPermute = newSwapsPermuteHelp swapElems
{-# INLINE newSwapsPermute #-}

unsafeNewSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
unsafeNewSwapsPermute = newSwapsPermuteHelp unsafeSwapElems
{-# INLINE unsafeNewSwapsPermute #-}

newSwapsPermuteHelp :: (MPermute p m) => (p -> Int -> Int -> m ())
                       -> Int -> [(Int,Int)] -> m p
newSwapsPermuteHelp swap n ss = do
    p <- newPermute n
    mapM_ (uncurry $ swap p) ss
    return $! p
{-# INLINE newSwapsPermuteHelp #-}

-- | Construct a new permutation by copying another.
newCopyPermute :: (MPermute p m) => p -> m p
newCopyPermute p = do
    n  <- getSize p
    p' <- newPermute_ n
    copyPermute p' p
    return $! p'
{-# INLINE newCopyPermute #-}

-- | @copyPermute dst src@ copies the elements of the permutation @src@
-- into the permtuation @dst@.  The two permutations must have the same
-- size.
copyPermute :: (MPermute p m) => p -> p -> m ()
copyPermute dst src =
    getElems src >>= setElems dst
{-# INLINE copyPermute #-}

-- | Set a permutation to the identity.
setIdentity :: (MPermute p m) => p -> m ()
setIdentity p = do
    n <- getSize p
    setElems p [0 .. n-1]
{-# INLINE setIdentity #-}

-- | @getElem p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
getElem :: (MPermute p m) => p -> Int -> m Int
getElem p i = do
    n <- getSize p
    when (i < 0 || i >= n) $ fail "getElem: invalid index"
    unsafeGetElem p i
{-# INLINE getElem #-}

-- | @setElem p i x@ sets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
setElem :: (MPermute p m) => p -> Int -> Int -> m ()
setElem p i x = do
    n <- getSize p
    when (i < 0 || i >= n) $ fail "getElem: invalid index"
    unsafeSetElem p i x
{-# INLINE setElem #-}

-- | @swapElems p i j@ exchanges the @i@th and @j@th elements of the 
-- permutation @p@.
swapElems :: (MPermute p m) => p -> Int -> Int -> m ()
swapElems p i j = do
    n <- getSize p
    when (i < 0 || i >= n || j < 0 || j >= n) $ fail "swapElems: invalid index"
    unsafeSwapElems p i j
{-# INLINE swapElems #-}

-- | Returns whether or not the permutation is valid.  For it to be valid,
-- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
-- values @p[0],...,p[n-1]@.
isValid :: (MPermute p m) => p -> m Bool
isValid p = do
    n <- getSize p
    liftM and $ validIndices n
  where
    j `existsIn` i = do
        seen <- liftM (take i) $ getElems p
        return $ (any (==j)) seen
        
    isValidIndex n i = do
        i' <- unsafeGetElem p i
        valid  <- return $ i' >= 0 && i' < n
        unique <- liftM not (i' `existsIn` i)
        return $ valid && unique

    validIndices n = validIndicesHelp n 0

    validIndicesHelp n i
        | i == n = return []
        | otherwise = do
            a  <- isValidIndex n i
            as <- unsafeInterleaveM $ validIndicesHelp n (i+1)
            return (a:as)
{-# INLINE isValid #-}

-- | Compute the inverse of a permutation.  
getInverse :: (MPermute p m) => p -> m p
getInverse p = do
    n <- getSize p
    q <- newPermute_ n
    copyInverse q p
    return $! q
{-# INLINE getInverse #-}

-- | Set one permutation to be the inverse of another.  
-- @copyInverse inv p@ computes the inverse of @p@ and stores it in @inv@.
-- The two permutations must have the same size.
copyInverse :: (MPermute p m) => p -> p -> m ()
copyInverse dst src = do
    n  <- getSize src
    n' <- getSize dst
    when (n /= n') $ fail "permutation size mismatch"
    forM_ [0 .. n-1] $ \i -> do
        i' <- unsafeGetElem src i
        unsafeSetElem dst i' i
{-# INLINE copyInverse #-}
    
-- | Advance a permutation to the next permutation in lexicogrphic order and
-- return @True@.  If no further permutaitons are available, return @False@ and
-- leave the permutation unmodified.  Starting with the idendity permutation 
-- and repeatedly calling @setNext@ will iterate through all permutations of a 
-- given size.
setNext :: (MPermute p m) => p -> m Bool
setNext = setNextBy compare
{-# INLINE setNext #-}

-- | Step backwards to the previous permutation in lexicographic order and
-- return @True@.  If there is no previous permutation, return @False@ and
-- leave the permutation unmodified.
setPrev :: (MPermute p m) => p -> m Bool
setPrev = setNextBy (flip compare)
{-# INLINE setPrev #-}

setNextBy :: (MPermute p m) => (Int -> Int -> Ordering) -> p -> m Bool
setNextBy cmp p = do
    n <- getSize p
    if n > 1
        then do
            findLastAscent (n-2) >>=
                maybe (return False) (\i -> do
                    i'     <- unsafeGetElem p i
                    i1'    <- unsafeGetElem p (i+1)
                    (k,k') <- findSmallestLargerThan n i' (i+2) (i+1) i1'
                    
                    -- swap i and k
                    unsafeSetElem p i k'
                    unsafeSetElem p k i'
                    
                    reverseElems (i+1) (n-1)
                    
                    return True
                )
        else 
            return False
        
  where
    i `lt` j = cmp i j == LT
    i `gt` j = cmp i j == GT
    
    findLastAscent i = do
        ascent <- isAscent i
        if ascent then return (Just i) else recurse
      where
        recurse = if i /= 0 then findLastAscent (i-1) else return Nothing 
    
    findSmallestLargerThan n i' j k k'
        | j < n = do
            j' <- unsafeGetElem p j
            if j' `gt` i' && j' `lt` k'
                then findSmallestLargerThan n i' (j+1) j j'
                else findSmallestLargerThan n i' (j+1) k k'
        | otherwise =
            return (k,k')
            
    isAscent i = liftM2 lt (unsafeGetElem p i) (unsafeGetElem p (i+1))
    
    reverseElems i j
        | i >= j = return ()
        | otherwise = do
            unsafeSwapElems p i j
            reverseElems (i+1) (j-1)
{-# INLINE setNextBy #-}  


-- | Get a lazy list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getSwaps = getSwapsHelp False
{-# INLINE getSwaps #-}

-- | Get a lazy list of swaps equivalent to the inverse of a permutation.
getInvSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getInvSwaps = getSwapsHelp True
{-# INLINE getInvSwaps #-}

getSwapsHelp :: (MPermute p m) => Bool -> p -> m [(Int,Int)]
getSwapsHelp inv p = do
    n <- getSize p
    liftM concat $
        forM [0..(n-1)] $ \i -> do
            k <- unsafeGetElem p i
            least <- isLeast i k
            if least 
                then do
                    i' <- unsafeGetElem p i
                    unsafeInterleaveM $ doCycle i i i'
                else
                    return []
  where
    isLeast i k 
        | k > i = do
            k' <- unsafeGetElem p k
            isLeast i k'
        | k < i     = return False
        | otherwise = return True
        
    doCycle start i i'
        | i' == start = return []
        | otherwise = do
            i'' <- unsafeGetElem p i'
            let s = if inv then (start,i') else (i,i')
            ss <- unsafeInterleaveM $ doCycle start i' i''
            return (s:ss)
{-# INLINE getSwapsHelp #-}

-- | Convert a mutable permutation to an immutable one.
freeze :: (MPermute p m) => p -> m Permute
freeze p = unsafeFreeze =<< newCopyPermute p
{-# INLINE freeze #-}

-- | Convert an immutable permutation to a mutable one.
thaw :: (MPermute p m) => Permute -> m p
thaw p = newCopyPermute =<< unsafeThaw p
{-# INLINE thaw #-}

-- | @getSort n xs@ sorts the first @n@ elements of @xs@ and returns a 
-- permutation which transforms @xs@ into sorted order.  The results are
-- undefined if @n@ is greater than the length of @xs@.  This is a special 
-- case of 'getSortBy'.
getSort :: (Ord a, MPermute p m) => Int -> [a] -> m ([a], p)
getSort = getSortBy compare
{-# INLINE getSort #-}
    
getSortBy :: (MPermute p m) => (a -> a -> Ordering) -> Int -> [a] -> m ([a], p)
getSortBy cmp n xs =
    let ys       = take n xs
        (is,ys') = (unzip . List.sortBy (cmp `on` snd) . zip [0..]) ys
    in liftM ((,) ys') $ unsafeNewListPermute n is
{-# INLINE getSortBy #-}

-- | @getOrder n xs@ returns a permutation which rearranges the first @n@
-- elements of @xs@ into ascending order. The results are undefined if @n@ is 
-- greater than the length of @xs@.  This is a special case of 'getOrderBy'.
getOrder :: (Ord a, MPermute p m) => Int -> [a] -> m p
getOrder = getOrderBy compare
{-# INLINE getOrder #-}

getOrderBy :: (MPermute p m) => (a -> a -> Ordering) -> Int -> [a] -> m p
getOrderBy cmp n xs =
    liftM snd $ getSortBy cmp n xs
{-# INLINE getOrderBy #-}

-- | @getRank n xs@ eturns a permutation, the inverse of which rearranges the 
-- first @n@ elements of @xs@ into ascending order. The returned permutation, 
-- @p@, has the property that @p[i]@ is the rank of the @i@th element of @xs@. 
-- The results are undefined if @n@ is greater than the length of @xs@.
-- This is a special case of 'getRankBy'.  
getRank :: (Ord a, MPermute p m) => Int -> [a] -> m p
getRank = getRankBy compare
{-# INLINE getRank #-}

getRankBy :: (MPermute p m) => (a -> a -> Ordering) -> Int -> [a] -> m p
getRankBy cmp n xs = do
    p <- getOrderBy cmp n xs
    getInverse p
{-# INLINE getRankBy #-}


--------------------------------- Instances ---------------------------------

instance MPermute (STPermute s) (ST s) where
    getSize = getSizeSTPermute
    {-# INLINE getSize #-}
    newPermute = newSTPermute
    {-# INLINE newPermute #-}
    newPermute_ = newSTPermute_
    {-# INLINE newPermute_ #-}
    unsafeGetElem = unsafeGetElemSTPermute
    {-# INLINE unsafeGetElem #-}
    unsafeSetElem = unsafeSetElemSTPermute
    {-# INLINE unsafeSetElem #-}
    unsafeSwapElems = unsafeSwapElemsSTPermute
    {-# INLINE unsafeSwapElems #-}
    getElems = getElemsSTPermute
    {-# INLINE getElems #-}
    setElems = setElemsSTPermute
    {-# INLINE setElems #-}
    unsafeFreeze = unsafeFreezeSTPermute
    {-# INLINE unsafeFreeze #-}
    unsafeThaw = unsafeThawSTPermute
    {-# INLINE unsafeThaw #-}
    unsafeInterleaveM = unsafeInterleaveST
    {-# INLINE unsafeInterleaveM #-}


instance MPermute IOPermute IO where
    getSize = getSizeIOPermute
    {-# INLINE getSize #-}
    newPermute = newIOPermute
    {-# INLINE newPermute #-}
    newPermute_ = newIOPermute_
    {-# INLINE newPermute_ #-}
    unsafeGetElem = unsafeGetElemIOPermute
    {-# INLINE unsafeGetElem #-}
    unsafeSetElem = unsafeSetElemIOPermute
    {-# INLINE unsafeSetElem #-}
    unsafeSwapElems = unsafeSwapElemsIOPermute
    {-# INLINE unsafeSwapElems #-}
    getElems = getElemsIOPermute
    {-# INLINE getElems #-}
    setElems = setElemsIOPermute
    {-# INLINE setElems #-}
    unsafeFreeze = unsafeFreezeIOPermute
    {-# INLINE unsafeFreeze #-}
    unsafeThaw = unsafeThawIOPermute
    {-# INLINE unsafeThaw #-}
    unsafeInterleaveM = unsafeInterleaveIO
    {-# INLINE unsafeInterleaveM #-}

