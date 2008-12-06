{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
        FlexibleContexts, Rank2Types, BangPatterns, CPP #-}
{-# OPTIONS_GHC -XMagicHash -XUnboxedTuples #-}
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

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 ( realWorld# )
import GHC.IOBase               ( IO(IO) )
#if __GLASGOW_HASKELL__ >= 605
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif
#endif

import Control.Monad
import Control.Monad.ST
import Data.Function( on )
import Data.List( sortBy )
import Foreign

import System.IO.Unsafe


class (Monad m) => UnsafeIOToM m where
    unsafeInterleaveM :: m a -> m a
    unsafeIOToM :: IO a -> m a

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- | Internally, a permutation of size @n@ is stored as an
-- @0@-based array of @n@ 'Int's.  The permutation represents a reordering of
-- the integers @0, ..., (n-1)@.  The @i@th element of the array stores
-- the value @p_i@. 
data PermuteData = PD {-# UNPACK #-}  !(ForeignPtr Int) -- payload
                      {-# UNPACK #-}  !Int -- size

newArray_ :: Int -> IO PermuteData
newArray_ n = do
#if __GLASGOW_HASKELL__ >= 605
    fp <- mallocPlainForeignPtrBytes (n * sizeOf (0 :: Int))
#else
    fp <- mallocForeignPtrBytes (n * sizeOf (0 :: Int))
#endif
    return $ PD fp n

newListArray :: Int -> [Int] -> IO PermuteData
newListArray n is = do
    p@(PD fp _) <- newArray_ n
    withForeignPtr fp $ flip pokeArray (take n is)
    return p

newCopyArray :: PermuteData -> IO PermuteData
newCopyArray p@(PD _ n) = do
    q <- newArray_ n
    unsafeCopy q p
    return q

unsafeCopy :: PermuteData -> PermuteData -> IO ()
unsafeCopy (PD dst n) (PD src _) =
    withForeignPtr dst $ \pDst ->
    withForeignPtr src $ \pSrc ->
        copyArray pDst pSrc n

permuteDataSize :: PermuteData -> Int
permuteDataSize (PD _ n) = n
{-# INLINE permuteDataSize #-}

withPermuteData :: PermuteData -> (Ptr Int -> Int -> IO a) -> IO a
withPermuteData (PD fp n) f =
    withForeignPtr fp $ \p -> f p n
{-# INLINE withPermuteData #-}

withPermutePtr :: PermuteData -> (Ptr Int -> IO a) -> IO a
withPermutePtr (PD fp _) f =
    withForeignPtr fp f
{-# INLINE withPermutePtr #-}

readArray :: PermuteData -> Int -> IO Int
readArray p@(PD _ n) i
    | i >= 0 && i < n =
        unsafeRead p i
    | otherwise =
        fail "invalid index"
{-# INLINE readArray #-}

unsafeRead :: PermuteData -> Int -> IO Int
unsafeRead p i = withPermutePtr p (flip peekElemOff i)
{-# INLINE unsafeRead #-}

inlineRead :: PermuteData -> Int -> Int
inlineRead p i = inlinePerformIO $! unsafeRead p i
{-# INLINE inlineRead #-}

readElems :: PermuteData -> IO [Int]
readElems = return . inlineElems

inlineElems :: PermuteData -> [Int]
inlineElems (PD fp n) = go start
  where 
    start = unsafeForeignPtrToPtr fp
    end   = start `advancePtr` n
    
    go !ptr | ptr == end = inlinePerformIO $ do
                 touchForeignPtr fp
                 return []
            | otherwise =
                 let e  = inlinePerformIO $ peek ptr
                     es = go (ptr `advancePtr` 1)
                 in e `seq` (e:es)

writeArray :: PermuteData -> Int -> Int -> IO ()
writeArray p@(PD _ n) i x
    | i >= 0 && i < n =
        unsafeWrite p i x
    | otherwise =
        fail "invalid index"
{-# INLINE writeArray #-}

unsafeWrite :: PermuteData -> Int -> Int -> IO ()
unsafeWrite p i x = withPermutePtr p $ \ptr -> (pokeElemOff ptr i x)
{-# INLINE unsafeWrite #-}

swapArray :: PermuteData -> Int -> Int -> IO ()
swapArray p@(PD _ n) i j
    | i == j =
        return ()
    | i >= 0 && i < n && j >= 0 && j < n =
        unsafeSwap p i j
    | otherwise =
        fail "invalid index"

unsafeSwap :: PermuteData -> Int -> Int -> IO ()
unsafeSwap pd i j = withPermutePtr pd $ \ptr -> do
    i' <- peekElemOff ptr i
    j' <- peekElemOff ptr j
    pokeElemOff ptr i j'
    pokeElemOff ptr j i'
{-# INLINE unsafeSwap #-}


--------------------------------- Permute ---------------------------------

-- | The immutable permutation data type.
newtype Permute = Permute PermuteData

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
listPermute n is = runST $
    unsafeFreeze =<< newListPermute n is

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
    unsafeFreeze =<< newInvSwapsPermute n ss

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
unsafeApply (Permute a) i = a `inlineRead` i
{-# INLINE unsafeApply #-}

-- | Get the size of the permutation.
size :: Permute -> Int
size (Permute (PD _ n)) = n
{-# INLINE size #-}

-- | Get a list of the permutation elements.
elems :: Permute -> [Int]
elems (Permute a) = inlineElems a
{-# INLINE elems #-}

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
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.
swaps :: Permute -> [(Int,Int)]
swaps p = runST $
    getSwaps =<< unsafeThaw p

-- | Get a list of swaps equivalent to the inverse of permutation.
invSwaps :: Permute -> [(Int,Int)]
invSwaps p = runST $
    getInvSwaps =<< unsafeThaw p

-- | Returns a permutation which rearranges its first argument into ascending 
-- order.  This is a special case of 'orderBy'.
order :: (Ord a) => [a] -> Permute
order xs = runST $ 
    unsafeFreeze =<< getOrder xs

orderBy :: (a -> a -> Ordering) -> [a] -> Permute
orderBy cmp xs = runST $
    unsafeFreeze =<< getOrderBy cmp xs

-- | Returns a permutation, the inverse of which rearranges its first argument 
-- into ascending order. The returned permutation, @p@, has the property that
-- @p[i]@ is the rank of the @i@th element of the passed-in list. This is a 
-- special case of 'rankBy'.
rank :: (Ord a) => [a] -> Permute
rank xs = runST $
    unsafeFreeze =<< getRank xs

rankBy :: (a -> a -> Ordering) -> [a] -> Permute
rankBy cmp xs = runST $
    unsafeFreeze =<< getRankBy cmp xs

instance Show Permute where
    show p = "listPermute " ++ show (size p) ++ " " ++ show (elems p)
    
instance Eq Permute where
    (==) p q = (size p == size q) && (elems p == elems q)


--------------------------------- MPermute --------------------------------


                       

-- | Class for representing a mutable permutation.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable permutation will be
-- manipulated.
class (UnsafeIOToM m) => MPermute p m | p -> m, m -> p where
    -- | Get the underlying array that stores the permutation
    toData :: p -> PermuteData
    
    -- | Create a permutation using the specified array.  The array must
    -- be @0@-based.
    fromData :: PermuteData -> p


-- | Create a new permutation initialized to be the identity.
newPermute :: (MPermute p m) => Int -> m p
newPermute n = unsafeIOToM $
    liftM fromData $ newListArray n [0..]

-- | Allocate a new permutation but do not initialize it.
newPermute_ :: (MPermute p m) => Int -> m p
newPermute_ n = unsafeIOToM $
    liftM fromData $ newArray_ n
        
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
    return p

unsafeNewListPermute :: (MPermute p m) => Int -> [Int] -> m p
unsafeNewListPermute n is = unsafeIOToM $
    liftM fromData $ newListArray n is


-- | Construct a permutation from a list of swaps.
-- @newInvSwapsPermute n ss@ creates a permuation of size @n@ given by the
-- /inverse/ of a sequence of swaps.
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
newCopyPermute p = unsafeIOToM $
    liftM fromData $ newCopyArray (toData p)

-- | @copyPermute dst src@ copies the elements of the permutation @src@
-- into the permtuation @dst@.  The two permutations must have the same
-- size.
copyPermute :: (MPermute p m) => p -> p -> m ()
copyPermute q p = unsafeIOToM $ 
    unsafeCopy dst src
  where
    src = toData p
    dst = toData q

-- | Set a permutation to the identity.
setIdentity :: (MPermute p m) => p -> m ()
setIdentity p = unsafeIOToM $ do
    forM_ [0..(n-1)] $ \i ->
        unsafeWrite arr i i
  where
    arr = toData p
    n   = inlineGetSize p

-- | @getElem p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
getElem :: (MPermute p m) => p -> Int -> m Int
getElem p i = unsafeIOToM $ readArray (toData p) i
{-# INLINE getElem #-}

unsafeGetElem :: (MPermute p m) => p -> Int -> m Int
unsafeGetElem p i = unsafeIOToM $ unsafeRead (toData p) i
{-# INLINE unsafeGetElem #-}

-- | @swapElems p i j@ exchanges the @i@th and @j@th elements of the 
-- permutation @p@.
swapElems :: (MPermute p m) => p -> Int -> Int -> m ()
swapElems p i j = unsafeIOToM $ swapArray (toData p) i j

unsafeSwapElems :: (MPermute p m) => p -> Int -> Int -> m ()
unsafeSwapElems p i j =
    unsafeIOToM $
        unsafeSwap (toData p) i j


-- | Get the size of a permutation.
getSize :: (MPermute p m) => p -> m Int
getSize = return . inlineGetSize
{-# INLINE getSize #-}

inlineGetSize :: (MPermute p m) => p -> Int
inlineGetSize = permuteDataSize . toData
{-# INLINE inlineGetSize #-}

-- | Get a lazy list of the permutation elements.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getElems :: (MPermute p m) => p -> m [Int]
getElems = unsafeIOToM . readElems . toData

-- | Returns whether or not the permutation is valid.  For it to be valid,
-- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
-- values @p[0],...,p[n-1]@.
isValid :: (MPermute p m) => p -> m Bool
isValid p = unsafeIOToM $ do
    liftM and validIndices
  where
    arr@(PD _ n) = toData p

    j `existsIn` i = do
        seen <- liftM (take i) $ readElems arr
        return $ (any (==j)) seen
        
    isValidIndex i = do
        i' <- unsafeRead arr i
        valid  <- return $ i' >= 0 && i' < n
        unique <- liftM not (i' `existsIn` i)
        return $ valid && unique

    validIndices = validIndicesHelp 0

    validIndicesHelp i
        | i == n = return []
        | otherwise = do
            a  <- isValidIndex i
            as <- unsafeInterleaveIO $ validIndicesHelp (i+1)
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
copyInverse q p = unsafeIOToM $ do
    when (n /= n') $ fail "permutation size mismatch"
    forM_ [0..(n-1)] $ \i -> do
        i' <- unsafeRead src i
        unsafeWrite dst i' i
  where
    src@(PD _ n)  = toData p
    dst@(PD _ n') = toData q
        
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
setNextBy lt p = unsafeIOToM $ do
    if n > 1
        then do
            findLastAscent (n-2) >>=
                maybe (return False) (\i -> do
                    i'     <- unsafeRead arr i
                    i1'    <- unsafeRead arr (i+1)
                    (k,k') <- findSmallestLargerThan i' (i+2) (i+1) i1'
                    
                    -- swap i and k
                    unsafeWrite arr i k'
                    unsafeWrite arr k i'
                    
                    reverseElems (i+1) (n-1)
                    
                    return True
                )
        else 
            return False
        
  where
    n   = inlineGetSize p
    arr = toData p
    i `gt` j = not (i `lt` j)
    
    findLastAscent i = do
        ascent <- isAscent i
        if ascent then return (Just i) else recurse
      where
        recurse = if i /= 0 then findLastAscent (i-1) else return Nothing 
    
    findSmallestLargerThan i' j k k'
        | j < n = do
            j' <- unsafeRead arr j
            if j' `gt` i' && j' `lt` k'
                then findSmallestLargerThan i' (j+1) j j'
                else findSmallestLargerThan i' (j+1) k k'
        | otherwise =
            return (k,k')
            
    isAscent i = liftM2 lt (unsafeRead arr i) (unsafeRead arr (i+1))
    
    reverseElems i j
        | i >= j = return ()
        | otherwise = do
            unsafeSwap arr i j
            reverseElems (i+1) (j-1)
{-# INLINE setNextBy #-}

    
-- | Get a lazy list of swaps equivalent to the permutation.  A result of
-- @[ (i0,j0), (i1,j1), ..., (ik,jk) ]@ means swap @i0 \<-> j0@, 
-- then @i1 \<-> j1@, and so on until @ik \<-> jk@.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.
getSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getSwaps = getSwapsHelp False

-- | Get a lazy list of swaps equivalent to the inverse of a permutation.
getInvSwaps :: (MPermute p m) => p -> m [(Int,Int)]
getInvSwaps = getSwapsHelp True

getSwapsHelp :: (MPermute p m) => Bool -> p -> m [(Int,Int)]
getSwapsHelp inv p = unsafeIOToM $ do
    liftM concat $
        forM [0..(n-1)] $ \i -> do
            k <- unsafeRead arr i
            least <- isLeast i k
            if least 
                then do
                    i' <- unsafeRead arr i
                    unsafeInterleaveM $ doCycle i i i'
                else
                    return []
        
  where
    arr = toData p
    n   = permuteDataSize arr
    
    isLeast i k 
        | k > i = do
            k' <- unsafeRead arr k
            isLeast i k'
        | k < i     = return False
        | otherwise = return True
        
    doCycle start i i'
        | i' == start = return []
        | otherwise = do
            i'' <- unsafeRead arr i'
            let s = if inv then (start,i') else (i,i')
            ss <- unsafeInterleaveM $ doCycle start i' i''
            return (s:ss)
        
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

-- | Convert a mutable permutation to an immutable one.
freeze :: (MPermute p m) => p -> m Permute
freeze = freezeHelp newCopyArray

unsafeFreeze :: (MPermute p m) => p -> m Permute
unsafeFreeze = freezeHelp return
{-# INLINE unsafeFreeze #-}

freezeHelp :: (MPermute p m) => (PermuteData -> IO PermuteData)
           -> p -> m Permute
freezeHelp f p = unsafeIOToM $
    (liftM Permute . f . toData) p
{-# INLINE freezeHelp #-}

-- | Convert an immutable permutation to a mutable one.
thaw :: (MPermute p m) => Permute -> m p
thaw = thawHelp newCopyArray

unsafeThaw :: (MPermute p m) => Permute -> m p
unsafeThaw = thawHelp return
{-# INLINE unsafeThaw #-}

thawHelp :: (MPermute p m) => (PermuteData -> IO PermuteData)
           -> Permute -> m p
thawHelp t (Permute p) = unsafeIOToM $ liftM fromData $ t p
{-# INLINE thawHelp #-}

--------------------------------- Instances ---------------------------------

-- | A mutable permutation that can be manipulated in the 'ST' monad. The
-- type argument @s@ is the state variable argument for the 'ST' type.
newtype STPermute s = STPermute PermuteData

instance UnsafeIOToM (ST s) where
    unsafeIOToM = unsafeIOToST
    {-# INLINE unsafeIOToM #-}
    unsafeInterleaveM = unsafeInterleaveST
    {-# INLINE unsafeInterleaveM #-}

instance MPermute (STPermute s) (ST s) where
    toData (STPermute a) = a
    {-# INLINE toData #-}

    fromData = STPermute
    {-# INLINE fromData #-}


-- | A mutable permutation that can be manipulated in the 'IO' monad.
newtype IOPermute = IOPermute PermuteData

instance UnsafeIOToM IO where
    unsafeIOToM = id
    {-# INLINE unsafeIOToM #-}
    unsafeInterleaveM = unsafeInterleaveIO
    {-# INLINE unsafeInterleaveM #-}

    
instance MPermute IOPermute IO where
    toData (IOPermute a) = a
    {-# INLINE toData #-}

    fromData = IOPermute
    {-# INLINE fromData #-}
