{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, 
        FlexibleContexts, Rank2Types #-}
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

import Data.Array.Base( unsafeAt, unsafeRead )
import Data.Array.MArray hiding ( unsafeFreeze, thaw, unsafeThaw )
import qualified Data.Array.MArray as MArray
import Data.Array.IO ( IOUArray )
import Data.Array.ST ( STUArray )
import Data.Array.Unboxed hiding ( elems )
import qualified Data.Array.Unboxed as Array



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
-- @swapsPermute n ss@ creats a permuation of size @n@ from the sequence
-- of swaps.  For example, if @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
swapsPermute :: Int -> [(Int,Int)] -> Permute
swapsPermute n ss = runST $
    unsafeFreeze =<< newSwapsPermute n ss

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

-- | Get a list of swaps equivalent to the permutation.  The returned list will
-- have length equal to the size of the permutation.  A result of
-- @[ i0, i1, ..., in1 ]@ means swap @0 <-> i0@, then @1 <-> i1@, and so on
-- until @(n-1) <-> in1@.
swaps :: Permute -> [Int]
swaps p = runST $
    getSwaps =<< unsafeThaw p

-- | Get a list of swaps equivalent to the inverse of permutation.  The 
-- returned list will have length equal to the size of the permutation.
invSwaps :: Permute -> [Int]
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

-- | Class for representing a mutable permutation.  The type is parameterized
-- over the type of the monad, @m@, in which the mutable permutation will be
-- manipulated.
class (HasPermuteArray p, MArray (PermuteArray p) Int m) 
    => MPermute p m | p -> m, m -> p where
    -- | Get the underlying array that stores the permutation
    toData :: p -> PermuteData p
    
    -- | Create a permutation using the specified array.  The array must
    -- be @0@-based.
    fromData :: PermuteData p -> p


-- | Create a new permutation initialized to be the identity.
newPermute :: (MPermute p m) => Int -> m p
newPermute n =
    liftM fromData $ newListArray (0,n-1) [0..]

-- | Allocate a new permutation but do not initialize it.
newPermute_ :: (MPermute p m) => Int -> m p
newPermute_ n = 
    liftM fromData $ newArray_ (0,n-1)
        
-- | Construct a permutation from a list of elements.  
-- @newListPermute n is@ creates a permuation of size @n@ with
-- the @i@th element equal to @is !! i@.  For the permutation to be valid,
-- the list @is@ must have length @n@ and contain the indices @0..(n-1)@ 
-- exactly once each.
newListPermute :: (MPermute p m) => Int -> [Int] -> m p
newListPermute n is =
    liftM fromData $ newListArray (0,n-1) is

-- | Construct a permutation from a list of swaps.
-- @newSwapsPermute n ss@ creats a permuation of size @n@ from the sequence
-- of swaps.  For example, if @ss@ is @[(i0,j0), (i1,j1), ..., (ik,jk)]@, the
-- sequence of swaps is
-- @i0 \<-> j0@, then 
-- @i1 \<-> j1@, and so on until
-- @ik \<-> jk@.
newSwapsPermute :: (MPermute p m) => Int -> [(Int,Int)] -> m p
newSwapsPermute = undefined

-- | Construct a new permutation by copying another.
newCopyPermute :: (MPermute p m, MPermute q m) => p -> m q
newCopyPermute = undefined

-- | @copyPermute dst src@ copies the elements of the permutation @src@
-- into the permtuation @dst@.  The two permutations must have the same
-- size.
copyPermute :: (MPermute q m, MPermute p m) => q -> p -> m ()
copyPermute = undefined

-- | Set a permutation to the identity.
setIdentity :: (MPermute p m) => p -> m ()
setIdentity = undefined

-- | @getPermute p i@ gets the value of the @i@th element of the permutation
-- @p@.  The index @i@ must be in the range @0..(n-1)@, where @n@ is the
-- size of the permutation.
getPermute :: (MPermute p m) => p -> Int -> m Int
getPermute = undefined
{-# INLINE getPermute #-}

unsafeGetPermute :: (MPermute p m) => p -> Int -> m Int
unsafeGetPermute = undefined
{-# INLINE unsafeGetPermute #-}

-- | @swapPermute p i j@ exchanges the @i@th and @j@th elements of the 
-- permutation @p@.
swapPermute :: (MPermute p m) => p -> Int -> Int -> m ()
swapPermute = undefined
{-# INLINE swapPermute #-}

-- | Get the size of a permutation.
getSize :: (MPermute p m) => p -> m Int
getSize p = liftM ((1+) . snd) $ getBounds (toData p)
{-# INLINE getSize #-}

-- | Get a lazy list of the permutation elements.  The laziness makes this
-- function slightly dangerous if you are modifying the permutation.  See also
-- 'getElems\''.
getElems :: (MPermute p m) => p -> m [Int]
getElems = undefined

-- | Get a strict list of the permutation elements.
getElems' :: (MPermute p m) => p -> m [Int]
getElems' = undefined

-- | Returns whether or not the permutation is valid.  For it to be valid,
-- the numbers @0,...,(n-1)@ must all appear exactly once in the stored
-- values @p[0],...,p[n-1]@.
isValid :: forall p m. (MPermute p m) => p -> m Bool
isValid p = do
    n <- getSize p
    liftM and $ mapM (isValidIndex n) [0..(n-1)]
  where
    arr = toData p

    j `existsIn` i = do
        seen <- mapM (unsafeRead arr) [0..(i-1)]
        return $ (any (==j)) seen
        
    isValidIndex n i = do
        i' <- unsafeRead arr i
        valid  <- return $ i' >= 0 && i' < n
        unique <- liftM not (i' `existsIn` i)
        return $ valid && unique


-- | Compute the inverse of a permutation.  
getInverse :: (MPermute p m, MPermute pi m) => p -> m pi
getInverse = undefined

-- | Set one permutation to be the inverse of another.  
-- @copyInverse inv p@ computes the inverse of @p@ and stores it in @inv@.
-- The two permutations must have the same size.
copyInverse :: (MPermute pi m, MPermute p m) => pi -> p -> m ()
copyInverse = undefined

-- | Advance a permutation to the next permutation in lexicogrphic order and
-- return @True@.  If no further permutaitons are available, return @False@ and
-- leave the permutation unmodified.  Starting with the idendity permutation 
-- and repeatedly calling @setNext@ will iterate through all permutations of a 
-- given size.
setNext :: (MPermute p m) => p -> m Bool
setNext = undefined

-- | Step backwards to the previous permutation in lexicographic order and
-- return @True@.  If there is no previous permutation, return @False@ and
-- leave the permutation unmodified.
setPrev :: (MPermute p m) => p -> m Bool
setPrev = undefined

-- | Get a list of swaps equivalent to the permutation.  The returned list will
-- have length equal to the size of the permutation.  A result of
-- @[ i0, i1, ..., in1 ]@ means swap @0 <-> i0@, then @1 <-> i1@, and so on
-- until @(n-1) <-> in1@.
getSwaps :: (MPermute p m) => p -> m [Int]
getSwaps = undefined

-- | Get a list of swaps equivalent to the inverse of permutation.  The 
-- returned list will have length equal to the size of the permutation.
getInvSwaps :: (MPermute p m) => p -> m [Int]
getInvSwaps = undefined

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
    
instance MPermute (STPermute s) (ST s) where
    toData (STPermute a) = a
    {-# INLINE toData #-}

    fromData = STPermute
    {-# INLINE fromData #-}


-- | A mutable permutation that can be manipulated in the 'IO' monad.
newtype IOPermute = IOPermute (IOUArray Int Int)

instance HasPermuteArray IOPermute where
    type PermuteArray IOPermute = IOUArray
    
instance MPermute IOPermute IO where
    toData (IOPermute a) = a
    {-# INLINE toData #-}

    fromData = IOPermute
    {-# INLINE fromData #-}
