{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.IntArray
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.IntArray (
    IntArray,
    STIntArray,

    numElements,
    unsafeAt,
    elems,

    newArray_,
    
    numElementsSTIntArray,
    getNumElements,
    unsafeRead,
    unsafeWrite,
    unsafeSwap,
    
    readElems,
    writeElems,
    
    unsafeFreeze,
    unsafeThaw,
    
    ) where

import GHC.Prim
import GHC.Types
import GHC.ST
import Foreign( sizeOf )

-----------------------------  Immutable arrays -----------------------------

data IntArray = IntArray !Int (ByteArray#)

{-# INLINE numElements #-}
numElements :: IntArray -> Int
numElements (IntArray n _) = n

{-# INLINE unsafeAt #-}
unsafeAt :: IntArray -> Int -> Int
unsafeAt (IntArray _ arr#) (I# i#) =
    case indexIntArray# arr# i# of { e# ->
    I# e# }

{-# INLINE elems #-}
elems :: IntArray -> [Int]
elems arr@(IntArray n _) =
    [ unsafeAt arr i | i <- [0 .. n-1]]


------------------------------  Mutable arrays ------------------------------

data STIntArray s = STIntArray !Int (MutableByteArray# s)

{-# INLINE newArray_ #-}
newArray_ :: Int -> ST s (STIntArray s)
newArray_ n@(I# n#) =
    ST $ \s1# ->
        case newByteArray# (n# *# sizeOfInt) s1# of { (# s2#, marr# #) ->
        (# s2#, STIntArray n marr# #) }
  where
    sizeOfInt = case sizeOf (0 :: Int) of (I# s#) -> s#
    
{-# INLINE numElementsSTIntArray #-}
numElementsSTIntArray :: STIntArray s -> Int
numElementsSTIntArray (STIntArray n _) = n

{-# INLINE getNumElements #-}
getNumElements :: STIntArray s -> ST s Int
getNumElements arr = return $! numElementsSTIntArray arr

{-# INLINE unsafeRead #-}
unsafeRead :: STIntArray s -> Int -> ST s Int
unsafeRead (STIntArray _ marr#) (I# i#) =
    ST $ \s1# -> 
        case readIntArray# marr# i# s1# of { (# s2#, e# #) ->
        let e = I# e# in
        (# s2#, e #) }

{-# INLINE unsafeWrite #-}
unsafeWrite :: STIntArray s -> Int -> Int -> ST s ()
unsafeWrite (STIntArray _ marr#) (I# i#) (I# e#) =
    ST $ \s1# -> 
        case writeIntArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeSwap #-}
unsafeSwap :: STIntArray s -> Int -> Int -> ST s ()
unsafeSwap (STIntArray _ marr#) (I# i#) (I# j#) =
    ST $ \s1# ->
        let doSwap =
                case readIntArray#  marr# i# s1# of { (# s2#, e# #) ->
                case readIntArray#  marr# j# s2# of { (# s3#, f# #) ->
                case writeIntArray# marr# i# f# s3# of { s4# ->
                     writeIntArray# marr# j# e# s4# }}} in
        if i# ==# j# then (# s1#, () #)
                     else case doSwap of { s2# ->
                          (# s2#, () #) }

{-# INLINE readElems #-}
readElems :: STIntArray s -> ST s [Int]
readElems (STIntArray (I# n#) marr#) =
    ST $ \s1# ->
        let inlineReadList i# | i# ==# n# = []
                              | otherwise = 
                case readIntArray# marr# i# s1# of { (# _, e# #) ->
                let e  = I# e#
                    es = inlineReadList (i# +# 1#) in
                (e:es) } in
        case inlineReadList 0# of { es ->
        (# s1#, es #)}
        
{-# INLINE writeElems #-}
writeElems :: STIntArray s -> [Int] -> ST s ()
writeElems (STIntArray (I# n#) marr#) es =
    ST $ \s1# ->
        let fillFromList i# xs s2# | i# ==# n# = s2#
                                   | otherwise = case xs of
                []         -> s2#
                (I# y#):ys -> case writeIntArray# marr# i# y# s2# of { s3# ->
                              fillFromList (i# +# 1#) ys s3# } in
        case fillFromList 0# es s1# of { s2# ->
        (# s2#, () #) }

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: STIntArray s -> ST s IntArray
unsafeFreeze (STIntArray n marr#) =
    ST $ \s1# ->
        case unsafeFreezeByteArray# marr# s1# of { (# s2#, arr# #) ->
        let arr = IntArray n arr# in
        (# s2#, arr #)}

{-# INLINE unsafeThaw #-}
unsafeThaw :: IntArray -> ST s (STIntArray s)
unsafeThaw (IntArray n arr#) =
    ST $ \s1# ->
        let coerceArray :: State# s -> MutableByteArray# s
            coerceArray _ = unsafeCoerce# arr#
            marr# = coerceArray s1#
            marr  = STIntArray n marr# in
        (# s1#, marr #)
