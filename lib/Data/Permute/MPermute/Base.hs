{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
        FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.MPermute.Base
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

-- #hide
module Data.Permute.MPermute.Base
    where

import Control.Monad.ST
import Data.Function( on )
import Data.List( sortBy )

import Data.Permute.Base
import Data.Permute.IOBase


--------------------------------- MPermute --------------------------------

-- | Class for representing a mutable permutation.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable permutation will be
-- manipulated.
class (Monad m) => MPermute p m | p -> m, m -> p where
    -- | Create a new permutation initialized to be the identity.
    newPermute :: (MPermute p m) => Int -> m p

    -- | Allocate a new permutation but do not initialize it.
    newPermute_ :: (MPermute p m) => Int -> m p
        
    -- | Construct a permutation from a list of elements.  
    -- @newListPermute n is@ creates a permuation of size @n@ with
    -- the @i@th element equal to @is !! i@.  For the permutation to be valid,
    -- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
    -- exactly once each.
    newListPermute :: (MPermute p m) => Int -> [Int] -> m p
    unsafeNewListPermute :: (MPermute p m) => Int -> [Int] -> m p

    -- | Construct a permutation from a list of swaps.
    -- @newInvSwapsPermute n ss@ creates a permuation of size @n@ given by the
    -- /inverse/ of a sequence of swaps.
    -- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
    -- sequence of swaps is
    -- @i0 \<-> j0@, then 
    -- @i1 \<-> j1@, and so on until
    -- @ik \<-> jk@.
    newInvSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
    unsafeNewInvSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p

    -- | Construct a new permutation by copying another.
    newCopyPermute :: (MPermute p m) => p -> m p

    -- | @copyPermute dst src@ copies the elements of the permutation @src@
    -- into the permtuation @dst@.  The two permutations must have the same
    -- size.
    copyPermute :: (MPermute p m) => p -> p -> m ()

    -- | Set a permutation to the identity.
    setIdentity :: (MPermute p m) => p -> m ()

    -- | @getElem p i@ gets the value of the @i@th element of the permutation
    -- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
    -- size of the permutation.
    getElem :: (MPermute p m) => p -> Int -> m Int
    unsafeGetElem :: (MPermute p m) => p -> Int -> m Int

    -- | @swapElems p i j@ exchanges the @i@th and @j@th elements of the 
    -- permutation @p@.
    swapElems :: (MPermute p m) => p -> Int -> Int -> m ()
    unsafeSwapElems :: (MPermute p m) => p -> Int -> Int -> m ()

    -- | Get the size of a permutation.
    getSize :: (MPermute p m) => p -> m Int

    -- | Get a lazy list of the permutation elements.  The laziness makes this
    -- function slightly dangerous if you are modifying the permutation.
    getElems :: (MPermute p m) => p -> m [Int]

    -- | Returns whether or not the permutation is valid.  For it to be valid,
    -- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
    -- values @p[0],...,p[n-1]@.
    isValid :: (MPermute p m) => p -> m Bool

    -- | Compute the inverse of a permutation.  
    getInverse :: (MPermute p m) => p -> m p

    -- | Set one permutation to be the inverse of another.  
    -- @copyInverse inv p@ computes the inverse of @p@ and stores it in @inv@.
    -- The two permutations must have the same size.
    copyInverse :: (MPermute p m) => p -> p -> m ()
        
    -- | Advance a permutation to the next permutation in lexicogrphic order and
    -- return @True@.  If no further permutaitons are available, return @False@ and
    -- leave the permutation unmodified.  Starting with the idendity permutation 
    -- and repeatedly calling @setNext@ will iterate through all permutations of a 
    -- given size.
    setNext :: (MPermute p m) => p -> m Bool

    -- | Step backwards to the previous permutation in lexicographic order and
    -- return @True@.  If there is no previous permutation, return @False@ and
    -- leave the permutation unmodified.
    setPrev :: (MPermute p m) => p -> m Bool

    -- | Get a lazy list of swaps equivalent to the permutation.  A result of
    -- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
    -- then @i1 \<-> j1@, and so on until @ik \<-> jk@.  The laziness makes this
    -- function slightly dangerous if you are modifying the permutation.
    getSwaps :: (MPermute p m) => p -> m [(Int,Int)]

    -- | Get a lazy list of swaps equivalent to the inverse of a permutation.
    getInvSwaps :: (MPermute p m) => p -> m [(Int,Int)]

    -- | Convert a mutable permutation to an immutable one.
    freeze :: (MPermute p m) => p -> m Permute
    unsafeFreeze :: (MPermute p m) => p -> m Permute

    -- | Convert an immutable permutation to a mutable one.
    thaw :: (MPermute p m) => Permute -> m p
    unsafeThaw :: (MPermute p m) => Permute -> m p

-- | Returns a permutation which rearranges its first argument into ascending 
-- order.  This is a special case of 'getOrderBy'.
getOrder :: (Ord a, MPermute p m) => [a] -> m p
getOrder = getOrderBy compare

getOrderBy :: (MPermute p m) => (a -> a -> Ordering) -> [a] -> m p
getOrderBy cmp xs =
    let is = (fst . unzip . sortBy (cmp `on` snd) . zip [0..]) xs
        n  = length xs
    in newListPermute n is

-- | Returns a permutation, the inverse of which rearranges its first argument 
-- into ascending order. The returned permutation, @p@, has the property that
-- @p[i]@ is the rank of the @i@th element of the passed-in list. This is a 
-- special case of 'getRankBy'.
getRank :: (Ord a, MPermute p m) => [a] -> m p
getRank = getRankBy compare

getRankBy :: (MPermute p m) => (a -> a -> Ordering) -> [a] -> m p
getRankBy cmp xs = do
    p <- getOrderBy cmp xs
    getInverse p


--------------------------------- Instances ---------------------------------

instance MPermute (STPermute s) (ST s) where
    newPermute = newSTPermute
    {-# INLINE newPermute #-}
    newPermute_ = newSTPermute_
    {-# INLINE newPermute_ #-}
    newListPermute = newListSTPermute
    {-# INLINE newListPermute #-}
    unsafeNewListPermute = unsafeNewListSTPermute
    {-# INLINE unsafeNewListPermute #-}
    newInvSwapsPermute = newInvSwapsSTPermute
    {-# INLINE newInvSwapsPermute #-}
    unsafeNewInvSwapsPermute = unsafeNewInvSwapsSTPermute
    {-# INLINE unsafeNewInvSwapsPermute #-}
    newCopyPermute = newCopySTPermute
    {-# INLINE newCopyPermute #-}
    copyPermute = copySTPermute
    {-# INLINE copyPermute #-}
    setIdentity = setIdentitySTPermute
    {-# INLINE setIdentity #-}
    getElem = getElemSTPermute
    {-# INLINE getElem #-}
    unsafeGetElem = unsafeGetElemSTPermute
    {-# INLINE unsafeGetElem #-}
    swapElems = swapElemsSTPermute
    {-# INLINE swapElems #-}
    unsafeSwapElems = unsafeSwapElemsSTPermute
    {-# INLINE unsafeSwapElems #-}
    getSize = getSizeSTPermute
    {-# INLINE getSize #-}
    getElems = getElemsSTPermute
    {-# INLINE getElems #-}
    isValid = isValidSTPermute
    {-# INLINE isValid #-}
    getInverse = getInverseSTPermute
    {-# INLINE getInverse #-}
    copyInverse = copyInverseSTPermute
    {-# INLINE copyInverse #-}
    setNext = setNextSTPermute
    {-# INLINE setNext #-}
    setPrev = setPrevSTPermute
    {-# INLINE setPrev #-}
    getSwaps = getSwapsSTPermute
    {-# INLINE getSwaps #-}
    getInvSwaps = getInvSwapsSTPermute
    {-# INLINE getInvSwaps #-}
    freeze = freezeSTPermute
    {-# INLINE freeze #-}
    unsafeFreeze = unsafeFreezeSTPermute
    {-# INLINE unsafeFreeze #-}
    thaw = thawSTPermute
    {-# INLINE thaw #-}
    unsafeThaw = unsafeThawSTPermute
    {-# INLINE unsafeThaw #-}


instance MPermute IOPermute IO where
    newPermute = newIOPermute
    {-# INLINE newPermute #-}
    newPermute_ = newIOPermute_
    {-# INLINE newPermute_ #-}
    newListPermute = newListIOPermute
    {-# INLINE newListPermute #-}
    unsafeNewListPermute = unsafeNewListIOPermute
    {-# INLINE unsafeNewListPermute #-}
    newInvSwapsPermute = newInvSwapsIOPermute
    {-# INLINE newInvSwapsPermute #-}
    unsafeNewInvSwapsPermute = unsafeNewInvSwapsIOPermute
    {-# INLINE unsafeNewInvSwapsPermute #-}
    newCopyPermute = newCopyIOPermute
    {-# INLINE newCopyPermute #-}
    copyPermute = copyIOPermute
    {-# INLINE copyPermute #-}
    setIdentity = setIdentityIOPermute
    {-# INLINE setIdentity #-}
    getElem = getElemIOPermute
    {-# INLINE getElem #-}
    unsafeGetElem = unsafeGetElemIOPermute
    {-# INLINE unsafeGetElem #-}
    swapElems = swapElemsIOPermute
    {-# INLINE swapElems #-}
    unsafeSwapElems = unsafeSwapElemsIOPermute
    {-# INLINE unsafeSwapElems #-}
    getSize = getSizeIOPermute
    {-# INLINE getSize #-}
    getElems = getElemsIOPermute
    {-# INLINE getElems #-}
    isValid = isValidIOPermute
    {-# INLINE isValid #-}
    getInverse = getInverseIOPermute
    {-# INLINE getInverse #-}
    copyInverse = copyInverseIOPermute
    {-# INLINE copyInverse #-}
    setNext = setNextIOPermute
    {-# INLINE setNext #-}
    setPrev = setPrevIOPermute
    {-# INLINE setPrev #-}
    getSwaps = getSwapsIOPermute
    {-# INLINE getSwaps #-}
    getInvSwaps = getInvSwapsIOPermute
    {-# INLINE getInvSwaps #-}
    freeze = freezeIOPermute
    {-# INLINE freeze #-}
    unsafeFreeze = unsafeFreezeIOPermute
    {-# INLINE unsafeFreeze #-}
    thaw = thawIOPermute
    {-# INLINE thaw #-}
    unsafeThaw = unsafeThawIOPermute
    {-# INLINE unsafeThaw #-}
