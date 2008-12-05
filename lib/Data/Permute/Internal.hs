{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, 
        FlexibleContexts, Rank2Types, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Permute.Internal
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Permute.Internal 
    where
    
import Control.Monad
import Control.Monad.ST

import Data.Array.Base ( unsafeAt, unsafeRead, unsafeWrite )
import Data.Array.MArray ( MArray )
import qualified Data.Array.MArray as MArray
import Data.Array.IO ( IOUArray )
import Data.Array.ST ( STUArray )
import Data.Array.Unboxed hiding ( elems )
import qualified Data.Array.Unboxed as Array

import System.IO.Unsafe


--------------------------------- Permute ---------------------------------

-- | The immutable permutation data type.
newtype Permute = Permute (UArray Int Int)

-- | Construct an identity permutation of the given size.
permute :: Int -> Permute
permute n = runST $
    unsafeFreeze =<< newPermute n

-- | Construct a permutation from a list of elements.  
-- @listPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
listPermute :: Int -> [Int] -> Permute
listPermute n is = runST $ do
    p <- newListPermute n is
    valid <- isValid p
    when (not valid) $ fail "invalid permutation"
    unsafeFreeze p

-- | Construct a permutation from a list of swaps.
-- @invSwapsPermute n ss@ creats a permuation of size @n@ given by the
-- *inverse* of a sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
invSwapsPermute :: Int -> [(Int,Int)] -> Permute
invSwapsPermute n ss = runST $
    unsafeFreeze =<< newInvSwapsPermute n ss

-- | @apply p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
apply :: Permute -> Int -> Int
apply (Permute a) i = a ! i
{-# INLINE apply #-}

unsafeApply :: Permute -> Int -> Int
unsafeApply (Permute a) i = a `unsafeAt` i
{-# INLINE unsafeApply #-}

-- | Get the size of the permutation.
size :: Permute -> Int
size (Permute a) = ((1+) . snd . bounds) a

-- | Get a list of the permutation elements.
elems :: Permute -> [Int]
elems (Permute a) = Array.elems a

-- | Get the inverse of a permutation
inverse :: Permute -> Permute
inverse p = runST $ 
    unsafeFreeze =<< getInverse =<< unsafeThaw p

-- | Return the next permutation in lexicographic order, or @Nothing@ if
-- there are no further permutations.  Starting with the identity permutation
-- and repeatedly calling this function will iterate through all permutations
-- of a given order.
next :: Permute -> Maybe Permute
next = nextPrevHelp setNext

-- | Return the previous permutation in lexicographic order, or @Nothing@
-- if there is no such permutation.
prev :: Permute -> Maybe Permute
prev = nextPrevHelp setPrev

nextPrevHelp :: (forall s. STPermute s -> ST s Bool) 
             -> Permute -> Maybe Permute
nextPrevHelp set p = runST $ do
    p' <- thaw p
    set p' >>= \valid ->
        if valid
            then liftM Just $ unsafeFreeze p'
            else return Nothing

-- | Get a list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 <-> j0@, 
-- then @i1 <-> j1@, and so on until @ik <-> jk@.
swaps :: Permute -> [(Int,Int)]
swaps p = runST $
    getSwaps =<< unsafeThaw p

-- | Get a list of swaps equivalent to the inverse of permutation.
invSwaps :: Permute -> [(Int,Int)]
invSwaps p = runST $
    getInvSwaps =<< unsafeThaw p


instance Show Permute where
    show p = "listPermute " ++ show (size p) ++ " " ++ show (elems p)
    
instance Eq Permute where
    (==) p q = (size p == size q) && (elems p == elems q)


--------------------------------- MPermute --------------------------------

-- | Class for the associated array type of the underlying storage for a 
-- permutation.  Internally, a permutation of size @n@ is stored as an
-- @0@-based array of @n@ 'Int's.  The permutation represents a reordering of
-- the integers @0, ..., (n-1)@.  The @i@th element of the array stores
-- the value @p_i@. 
class HasPermuteArray p where
    -- | The underlying array type.
    type PermuteArray p :: * -> * -> *

-- | The 'Int' array type associated with a permutation type @p@.
type PermuteData p = PermuteArray p Int Int

class (Monad m) => UnsafeInterleaveM m where
    unsafeInterleaveM :: m a -> m a

-- | Class for representing a mutable permutation.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable permutation will be
-- manipulated.
class (HasPermuteArray p, UnsafeInterleaveM m, MArray (PermuteArray p) Int m) 
    => MPermute p m | p -> m, m -> p where
    -- | Get the underlying array that stores the permutation
    toData :: p -> PermuteData p
    
    -- | Create a permutation using the specified array.  The array must
    -- be @0@-based.
    fromData :: PermuteData p -> p


-- | Create a new permutation initialized to be the identity.
newPermute :: (MPermute p m) => Int -> m p
newPermute n =
    liftM fromData $ MArray.newListArray (0,n-1) [0..]

-- | Allocate a new permutation but do not initialize it.
newPermute_ :: (MPermute p m) => Int -> m p
newPermute_ n = 
    liftM fromData $ MArray.newArray_ (0,n-1)
        
-- | Construct a permutation from a list of elements.  
-- @newListPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
newListPermute :: (MPermute p m) => Int -> [Int] -> m p
newListPermute n is =
    liftM fromData $ MArray.newListArray (0,n-1) is

-- | Construct a permutation from a list of swaps.
-- @newInvSwapsPermute n ss@ creates a permuation of size @n@ given by the
-- *inverse* of a sequence of swaps.
-- If @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
newInvSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
newInvSwapsPermute = newInvSwapsPermuteHelp swapElems

unsafeNewInvSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
unsafeNewInvSwapsPermute = newInvSwapsPermuteHelp unsafeSwapElems

newInvSwapsPermuteHelp :: (MPermute p m) 
                       => (p -> Int -> Int -> m())
                       -> Int -> [(Int,Int)] -> m p
newInvSwapsPermuteHelp swap n ss = do
    p <- newPermute n
    mapM_ (uncurry $ swap p) ss
    return p

-- | Construct a new permutation by copying another.
newCopyPermute :: (MPermute p m) => p -> m p
newCopyPermute p =
    liftM fromData $ copyArray (toData p)
  where
    copyArray = MArray.mapArray id

-- | @copyPermute dst src@ copies the elements of the permutation @src@
-- into the permtuation @dst@.  The two permutations must have the same
-- size.
copyPermute :: (MPermute p m) => p -> p -> m ()
copyPermute q p = do
    n <- getSize p
    forM_ [0..(n-1)] $ \i ->
        unsafeRead src i >>= unsafeWrite dst i
  where
    src = toData p
    dst = toData q
    

-- | Set a permutation to the identity.
setIdentity :: (MPermute p m) => p -> m ()
setIdentity p = do
    n <- getSize p
    forM_ [0..(n-1)] $ \i ->
        unsafeWrite arr i i
  where
    arr = toData p

-- | @getElem p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
getElem :: (MPermute p m) => p -> Int -> m Int
getElem p i = MArray.readArray (toData p) i
{-# INLINE getElem #-}

unsafeGetElem :: (MPermute p m) => p -> Int -> m Int
unsafeGetElem p i = unsafeRead (toData p) i
{-# INLINE unsafeGetElem #-}

-- | @swapElems p i j@ exchanges the @i@th and @j@th elements of the 
-- permutation @p@.
swapElems :: (MPermute p m) => p -> Int -> Int -> m ()
swapElems = swapElemsHelp MArray.readArray MArray.writeArray

unsafeSwapElems :: (MPermute p m) => p -> Int -> Int -> m ()
unsafeSwapElems = swapElemsHelp unsafeRead unsafeWrite

swapElemsHelp :: (MPermute p m) 
              => (PermuteData p -> Int -> m Int)
              -> (PermuteData p -> Int -> Int -> m ())
              -> p -> Int -> Int -> m ()
swapElemsHelp read write p i j 
    | i /= j = do
        i' <- read arr i
        j' <- read arr j
        write arr j i'
        write arr i j'
    | otherwise =
        return ()
  where
    arr = toData p


-- | Get the size of a permutation.
getSize :: (MPermute p m) => p -> m Int
getSize p = liftM ((1+) . snd) $ MArray.getBounds (toData p)
{-# INLINE getSize #-}

-- | Get a lazy list of the permutation elements.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getElems :: (MPermute p m) => p -> m [Int]
getElems p = do
    n <- getSize p
    getElemsArr (toData p) n 0
  where
    getElemsArr arr n i 
        | i == n = return []
        | otherwise = do
            a  <- unsafeInterleaveM $ unsafeRead arr i
            as <- unsafeInterleaveM $ getElemsArr arr n (i+1)
            return (a:as)

-- | Returns whether or not the permutation is valid.  For it to be valid,
-- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
-- values @p[0],...,p[n-1]@.
isValid :: forall p m. (MPermute p m) => p -> m Bool
isValid p = do
    n <- getSize p
    liftM and (validIndices n)
  where
    arr = toData p

    j `existsIn` i = do
        seen <- liftM (take i) $ getElems p
        return $ (any (==j)) seen
        
    isValidIndex n i = do
        i' <- unsafeRead arr i
        valid  <- return $ i' >= 0 && i' < n
        unique <- liftM not (i' `existsIn` i)
        return $ valid && unique

    validIndices n = validIndicesHelp n 0

    validIndicesHelp n i
        | n == i = return []
        | otherwise = do
            a  <- isValidIndex n i
            as <- unsafeInterleaveM $ validIndicesHelp n (i+1)
            return (a:as)

-- | Compute the inverse of a permutation.  
getInverse :: (MPermute p m) => p -> m p
getInverse p = do
    n <- getSize p
    q <- newPermute_ n
    copyInverse q p
    return q

-- | Set one permutation to be the inverse of another.  
-- @copyInverse inv p@ computes the inverse of @p@ and stores it in @inv@.
-- The two permutations must have the same size.
copyInverse :: (MPermute p m) => p -> p -> m ()
copyInverse q p = do
    n  <- getSize p
    n' <- getSize q
    when (n /= n') $ fail "permutation size mismatch"
    forM_ [0..(n-1)] $ \i -> do
        i' <- unsafeRead src i
        unsafeWrite dst i' i
  where
    src = toData p
    dst = toData q
        
-- | Advance a permutation to the next permutation in lexicogrphic order and
-- return @True@.  If no further permutaitons are available, return @False@ and
-- leave the permutation unmodified.  Starting with the idendity permutation 
-- and repeatedly calling @setNext@ will iterate through all permutations of a 
-- given size.
setNext :: (MPermute p m) => p -> m Bool
setNext = setNextBy (<)

-- | Step backwards to the previous permutation in lexicographic order and
-- return @True@.  If there is no previous permutation, return @False@ and
-- leave the permutation unmodified.
setPrev :: (MPermute p m) => p -> m Bool
setPrev = setNextBy (>)

setNextBy :: (MPermute p m) => (Int -> Int -> Bool) -> p -> m Bool
setNextBy lt p = do
    n <- getSize p
    if n > 1
        then do
            findLastAscent (n-2) >>=
                maybe (return False) (\i -> do
                    i'     <- unsafeRead arr i
                    i1'    <- unsafeRead arr (i+1)
                    (k,k') <- findSmallestLargerThan n i' (i+2) (i+1) i1'
                    
                    -- swap i and k
                    unsafeWrite arr i k'
                    unsafeWrite arr k i'
                    
                    forM_ [(i+1)..((n+i) `div` 2)] $ \j ->
                        unsafeSwapElems p j (n + i - j)
                    
                    return True
                )
        else 
            return False
        
  where
    arr = toData p
    i `gt` j = not (i `lt` j)
    
    findLastAscent i = do
        ascent <- isAscent i
        if ascent then return (Just i) else recurse
      where
        recurse = if i /= 0 then findLastAscent (i-1) else return Nothing 
    
    findSmallestLargerThan !n !i' !j !k !k'
        | j < n = do
            j' <- unsafeRead arr j
            if j' `gt` i' && j' `lt` k'
                then findSmallestLargerThan n i' (j+1) j j'
                else findSmallestLargerThan n i' (j+1) k k'
        | otherwise =
            return (k,k')
            
    isAscent i = liftM2 lt (unsafeRead arr i) (unsafeRead arr (i+1))
    
    
-- | Get a lazy list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 <-> j0@, 
-- then @i1 <-> j1@, and so on until @ik <-> jk@.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getSwaps = getSwapsHelp False

-- | Get a lazy list of swaps equivalent to the inverse of a permutation.
getInvSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getInvSwaps = getSwapsHelp True

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
        


-- | Convert a mutable permutation to an immutable one.
freeze :: (MPermute p m) => p -> m Permute
freeze = freezeHelp MArray.freeze

unsafeFreeze :: (MPermute p m) => p -> m Permute
unsafeFreeze = freezeHelp MArray.unsafeFreeze

freezeHelp :: (MPermute p m) => (PermuteData p -> m (UArray Int Int))
           -> p -> m Permute
freezeHelp f = liftM Permute . f . toData

-- | Convert an immutable permutation to a mutable one.
thaw :: (MPermute p m) => Permute -> m p
thaw = thawHelp MArray.thaw

unsafeThaw :: (MPermute p m) => Permute -> m p
unsafeThaw = thawHelp MArray.unsafeThaw

thawHelp :: (MPermute p m) => (UArray Int Int -> m (PermuteData p))
           -> Permute -> m p
thawHelp t (Permute p) =
    liftM fromData $ t p


--------------------------------- Instances ---------------------------------

-- | A mutable permutation that can be manipulated in the 'ST' monad. The
-- type argument @s@ is the state variable argument for the 'ST' type.
newtype STPermute s = STPermute (STUArray s Int Int)

instance HasPermuteArray (STPermute s) where
    type PermuteArray (STPermute s) = STUArray s
    
instance UnsafeInterleaveM (ST s) where
    unsafeInterleaveM = unsafeInterleaveST
    {-# INLINE unsafeInterleaveM #-}

instance MPermute (STPermute s) (ST s) where
    toData (STPermute a) = a
    {-# INLINE toData #-}

    fromData = STPermute
    {-# INLINE fromData #-}


-- | A mutable permutation that can be manipulated in the 'IO' monad.
newtype IOPermute = IOPermute (IOUArray Int Int)

instance HasPermuteArray IOPermute where
    type PermuteArray IOPermute = IOUArray

instance UnsafeInterleaveM IO where
    unsafeInterleaveM = unsafeInterleaveIO
    {-# INLINE unsafeInterleaveM #-}
    
instance MPermute IOPermute IO where
    toData (IOPermute a) = a
    {-# INLINE toData #-}

    fromData = IOPermute
    {-# INLINE fromData #-}
