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

-- | Construct an identity permutation of the given size.
permute :: Int -> Permute
permute n = runST $
    unsafeFreezeSTPermute =<< newSTPermute n

-- | Construct a permutation from a list of elements.  
-- @listPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
listPermute :: Int -> [Int] -> Permute
listPermute n is = runST $
    unsafeFreezeSTPermute =<< newListSTPermute n is

-- | Construct a permutation from a list of swaps.
-- @invSwapsPermute n ss@ creats a permuation of size @n@ given by the
-- /inverse/ of a sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
invSwapsPermute :: Int -> [(Int,Int)] -> Permute
invSwapsPermute n ss = runST $
    unsafeFreezeSTPermute =<< newInvSwapsSTPermute n ss

-- | @apply p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
apply :: Permute -> Int -> Int
apply p i
    | i >= 0 && i < size p = 
        unsafeApply p i
    | otherwise =
        error "Invalid index"
{-# INLINE apply #-}

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

-- | Get the inverse of a permutation
inverse :: Permute -> Permute
inverse p = runST $ 
    unsafeFreezeSTPermute =<< getInverseSTPermute =<< unsafeThawSTPermute p

-- | Return the next permutation in lexicographic order, or @Nothing@ if
-- there are no further permutations.  Starting with the identity permutation
-- and repeatedly calling this function will iterate through all permutations
-- of a given order.
next :: Permute -> Maybe Permute
next = nextPrevHelp setNextSTPermute

-- | Return the previous permutation in lexicographic order, or @Nothing@
-- if there is no such permutation.
prev :: Permute -> Maybe Permute
prev = nextPrevHelp setPrevSTPermute

nextPrevHelp :: (forall s. STPermute s -> ST s Bool) 
             -> Permute -> Maybe Permute
nextPrevHelp set p = runST $ do
    p' <- thawSTPermute p
    set p' >>= \valid ->
        if valid
            then liftM Just $ unsafeFreezeSTPermute p'
            else return Nothing

-- | Get a list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.
swaps :: Permute -> [(Int,Int)]
swaps p = runST $
    getSwapsSTPermute =<< unsafeThawSTPermute p

-- | Get a list of swaps equivalent to the inverse of permutation.
invSwaps :: Permute -> [(Int,Int)]
invSwaps p = runST $
    getInvSwapsSTPermute =<< unsafeThawSTPermute p

instance Show Permute where
    show p = "listPermute " ++ show (size p) ++ " " ++ show (elems p)
    
instance Eq Permute where
    (==) p q = (size p == size q) && (elems p == elems q)


--------------------------------- STPermute --------------------------------

-- | A mutable permutation that can be manipulated in the 'ST' monad. The
-- type argument @s@ is the state variable argument for the 'ST' type.
newtype STPermute s = STPermute (STIntArray s)

-- | Create a new permutation initialized to be the identity.
newSTPermute :: Int -> ST s (STPermute s)
newSTPermute n = do
    p <- newSTPermute_ n
    setIdentitySTPermute p
    return $! p
{-# INLINE newSTPermute #-}

-- | Allocate a new permutation but do not initialize it.
newSTPermute_ :: Int -> ST s (STPermute s)
newSTPermute_ n = liftM STPermute $ ArrST.newArray_ n
{-# INLINE newSTPermute_ #-}
        
-- | Construct a permutation from a list of elements.  
-- @newListPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
newListSTPermute :: Int -> [Int] -> ST s (STPermute s)
newListSTPermute n is = do
    p <- unsafeNewListSTPermute n is
    valid <- isValidSTPermute p
    when (not valid) $ fail "invalid permutation"
    return $! p

unsafeNewListSTPermute :: Int -> [Int] -> ST s (STPermute s)
unsafeNewListSTPermute n is = 
    liftM STPermute $ ArrST.newListArray n is

-- | Construct a permutation from a list of swaps.
-- @newInvSwapsPermute n ss@ creates a permuation of size @n@ given by the
-- /inverse/ of a sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
newInvSwapsSTPermute :: Int -> [(Int,Int)] -> ST s (STPermute s)
newInvSwapsSTPermute = newInvSwapsSTPermuteHelp swapElemsSTPermute
{-# NOINLINE newInvSwapsSTPermute #-}

unsafeNewInvSwapsSTPermute :: Int -> [(Int,Int)] -> ST s (STPermute s)
unsafeNewInvSwapsSTPermute = newInvSwapsSTPermuteHelp unsafeSwapElemsSTPermute
{-# NOINLINE unsafeNewInvSwapsSTPermute #-}

newInvSwapsSTPermuteHelp :: (STPermute s -> Int -> Int -> ST s ())
                       -> Int -> [(Int,Int)] -> ST s (STPermute s)
newInvSwapsSTPermuteHelp swap n ss = do
    p <- newSTPermute n
    mapM_ (uncurry $ swap p) ss
    return p
{-# INLINE newInvSwapsSTPermuteHelp #-}

-- | Construct a new permutation by copying another.
newCopySTPermute :: STPermute s -> ST s (STPermute s)
newCopySTPermute (STPermute p) =
    liftM STPermute $ ArrST.newCopyArray p
{-# INLINE newCopySTPermute #-}

-- | @copyPermute dst src@ copies the elements of the permutation @src@
-- into the permtuation @dst@.  The two permutations must have the same
-- size.
copySTPermute :: STPermute s -> STPermute s -> ST s ()
copySTPermute (STPermute dst) (STPermute src) =
    ArrST.copyArray dst src
{-# INLINE copySTPermute #-}

-- | Set a permutation to the identity.
setIdentitySTPermute :: STPermute s -> ST s ()
setIdentitySTPermute (STPermute p) = let
    n = ArrST.numElementsSTIntArray p
    in ArrST.writeElems p [0 .. n-1]
{-# INLINE setIdentitySTPermute #-}

-- | @getElem p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
getElemSTPermute :: STPermute s -> Int -> ST s Int
getElemSTPermute (STPermute p) i = ArrST.readArray p i
{-# INLINE getElemSTPermute #-}

unsafeGetElemSTPermute :: STPermute s -> Int -> ST s Int
unsafeGetElemSTPermute (STPermute p) i = ArrST.unsafeRead p i
{-# INLINE unsafeGetElemSTPermute #-}

-- | @swapElems p i j@ exchanges the @i@th and @j@th elements of the 
-- permutation @p@.
swapElemsSTPermute :: STPermute s -> Int -> Int -> ST s ()
swapElemsSTPermute (STPermute p) i j = ArrST.swapArray p i j
{-# INLINE swapElemsSTPermute #-}

unsafeSwapElemsSTPermute :: STPermute s -> Int -> Int -> ST s ()
unsafeSwapElemsSTPermute (STPermute p) i j =
    ArrST.unsafeSwap p i j
{-# INLINE unsafeSwapElemsSTPermute #-}

-- | Get the size of a permutation.
getSizeSTPermute :: STPermute s -> ST s Int
getSizeSTPermute p = return $! sizeSTPermute p
{-# INLINE getSizeSTPermute #-}

sizeSTPermute :: STPermute s -> Int
sizeSTPermute (STPermute p) = ArrST.numElementsSTIntArray p
{-# INLINE sizeSTPermute #-}

-- | Get a lazy list of the permutation elements.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getElemsSTPermute :: STPermute s -> ST s [Int]
getElemsSTPermute (STPermute p) = ArrST.readElems p
{-# INLINE getElemsSTPermute #-}

-- | Returns whether or not the permutation is valid.  For it to be valid,
-- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
-- values @p[0],...,p[n-1]@.
isValidSTPermute :: STPermute s -> ST s Bool
isValidSTPermute (STPermute arr) =
    liftM and validIndices
  where
    n = ArrST.numElementsSTIntArray arr
    
    j `existsIn` i = do
        seen <- liftM (take i) $ ArrST.readElems arr
        return $ (any (==j)) seen
        
    isValidIndex i = do
        i' <- ArrST.unsafeRead arr i
        valid  <- return $ i' >= 0 && i' < n
        unique <- liftM not (i' `existsIn` i)
        return $ valid && unique

    validIndices = validIndicesHelp 0

    validIndicesHelp i
        | i == n = return []
        | otherwise = do
            a  <- isValidIndex i
            as <- unsafeInterleaveST $ validIndicesHelp (i+1)
            return (a:as)

-- | Compute the inverse of a permutation.  
getInverseSTPermute :: STPermute s -> ST s (STPermute s)
getInverseSTPermute p = let
    n = sizeSTPermute p 
    in do
        q <- newSTPermute_ n
        copyInverseSTPermute q p
        return $! q

-- | Set one permutation to be the inverse of another.  
-- @copyInverse inv p@ computes the inverse of @p@ and stores it in @inv@.
-- The two permutations must have the same size.
copyInverseSTPermute :: STPermute s -> STPermute s -> ST s ()
copyInverseSTPermute (STPermute dst) (STPermute src) = do
    when (n /= n') $ fail "permutation size mismatch"
    forM_ [0..(n-1)] $ \i -> do
        i' <- ArrST.unsafeRead src i
        ArrST.unsafeWrite dst i' i
  where
    n  = ArrST.numElementsSTIntArray src
    n' = ArrST.numElementsSTIntArray dst
{-# INLINE copyInverseSTPermute #-}
        
-- | Advance a permutation to the next permutation in lexicogrphic order and
-- return @True@.  If no further permutaitons are available, return @False@ and
-- leave the permutation unmodified.  Starting with the idendity permutation 
-- and repeatedly calling @setNext@ will iterate through all permutations of a 
-- given size.
setNextSTPermute :: STPermute s -> ST s Bool
setNextSTPermute = inlineSetNextBySTPermute compare
{-# NOINLINE setNextSTPermute #-}

-- | Step backwards to the previous permutation in lexicographic order and
-- return @True@.  If there is no previous permutation, return @False@ and
-- leave the permutation unmodified.
setPrevSTPermute :: STPermute s -> ST s Bool
setPrevSTPermute = inlineSetNextBySTPermute (flip compare)
{-# NOINLINE setPrevSTPermute #-}

setNextBySTPermute :: (Int -> Int -> Ordering) -> STPermute s -> ST s Bool
setNextBySTPermute = inlineSetNextBySTPermute
{-# NOINLINE setNextBySTPermute #-}

inlineSetNextBySTPermute :: (Int -> Int -> Ordering) -> STPermute s -> ST s Bool
inlineSetNextBySTPermute cmp (STPermute arr) =
    if n > 1
        then do
            findLastAscent (n-2) >>=
                maybe (return False) (\i -> do
                    i'     <- ArrST.unsafeRead arr i
                    i1'    <- ArrST.unsafeRead arr (i+1)
                    (k,k') <- findSmallestLargerThan i' (i+2) (i+1) i1'
                    
                    -- swap i and k
                    ArrST.unsafeWrite arr i k'
                    ArrST.unsafeWrite arr k i'
                    
                    reverseElems (i+1) (n-1)
                    
                    return True
                )
        else 
            return False
        
  where
    n   = ArrST.numElementsSTIntArray arr
    i `lt` j = cmp i j == LT
    i `gt` j = cmp i j == GT
    
    findLastAscent i = do
        ascent <- isAscent i
        if ascent then return (Just i) else recurse
      where
        recurse = if i /= 0 then findLastAscent (i-1) else return Nothing 
    
    findSmallestLargerThan i' j k k'
        | j < n = do
            j' <- ArrST.unsafeRead arr j
            if j' `gt` i' && j' `lt` k'
                then findSmallestLargerThan i' (j+1) j j'
                else findSmallestLargerThan i' (j+1) k k'
        | otherwise =
            return (k,k')
            
    isAscent i = liftM2 lt (ArrST.unsafeRead arr i) (ArrST.unsafeRead arr (i+1))
    
    reverseElems i j
        | i >= j = return ()
        | otherwise = do
            ArrST.unsafeSwap arr i j
            reverseElems (i+1) (j-1)
{-# INLINE inlineSetNextBySTPermute #-}  
  
    
-- | Get a lazy list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getSwapsSTPermute :: STPermute s -> ST s [(Int,Int)]
getSwapsSTPermute = getSwapsHelp False
{-# NOINLINE getSwapsSTPermute #-}

-- | Get a lazy list of swaps equivalent to the inverse of a permutation.
getInvSwapsSTPermute :: STPermute s -> ST s [(Int,Int)]
getInvSwapsSTPermute = getSwapsHelp True
{-# NOINLINE getInvSwapsSTPermute #-}

getSwapsHelp :: Bool -> STPermute s -> ST s [(Int,Int)]
getSwapsHelp inv (STPermute arr) = do
    liftM concat $
        forM [0..(n-1)] $ \i -> do
            k <- ArrST.unsafeRead arr i
            least <- isLeast i k
            if least 
                then do
                    i' <- ArrST.unsafeRead arr i
                    unsafeInterleaveST $ doCycle i i i'
                else
                    return []
        
  where
    n = ArrST.numElementsSTIntArray arr
    
    isLeast i k 
        | k > i = do
            k' <- ArrST.unsafeRead arr k
            isLeast i k'
        | k < i     = return False
        | otherwise = return True
        
    doCycle start i i'
        | i' == start = return []
        | otherwise = do
            i'' <- ArrST.unsafeRead arr i'
            let s = if inv then (start,i') else (i,i')
            ss <- unsafeInterleaveST $ doCycle start i' i''
            return (s:ss)
{-# INLINE getSwapsHelp #-}

-- | Convert a mutable permutation to an immutable one.
freezeSTPermute :: STPermute s -> ST s Permute
freezeSTPermute = freezeHelp ArrST.freeze
{-# INLINE freezeSTPermute #-}

unsafeFreezeSTPermute :: STPermute s -> ST s Permute
unsafeFreezeSTPermute = freezeHelp ArrST.unsafeFreeze
{-# INLINE unsafeFreezeSTPermute #-}

freezeHelp :: (STIntArray s -> ST s IntArray)
           -> STPermute s -> ST s Permute
freezeHelp f (STPermute p) = (liftM Permute . f) p
{-# INLINE freezeHelp #-}

-- | Convert an immutable permutation to a mutable one.
thawSTPermute :: Permute -> ST s (STPermute s)
thawSTPermute = thawHelp ArrST.thaw
{-# INLINE thawSTPermute #-}

unsafeThawSTPermute :: Permute -> ST s (STPermute s)
unsafeThawSTPermute = thawHelp ArrST.unsafeThaw
{-# INLINE unsafeThawSTPermute #-}

thawHelp :: (IntArray -> ST s (STIntArray s))
           -> Permute -> ST s (STPermute s)
thawHelp t (Permute p) = liftM STPermute $ t p
{-# INLINE thawHelp #-}


