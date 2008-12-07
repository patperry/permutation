module Main where
    
import Control.Monad
import Control.Monad.ST
import Data.List( permutations )
import Data.STRef
import System.Environment

import Data.Permute
import Data.Permute.MPermute
    
-- | Execute an action on every permutation of a given order.  This function
-- is unsafe, because it only allocates space for a single permutation.  The
-- action @f@ should not retain any references to the passed-in @Permute@
-- object, otherwise bad things will happen.  For instance, running
-- >
-- >    forAllPermutes 2 id
-- >
-- in ghci yields @[listPermute 2 [1,0],listPermute 2 [1,0]]@.
--
forAllPermutes :: Int -> (Permute -> a) -> [a]
forAllPermutes n f = runST $ do
    -- Allocate a mutable permutation initialized to the identity
    p  <- newPermute n
    
    -- Run the action on all successors of p
    runOnSuccessors p
    
  where
    runOnSuccessors p = do
        -- Cast the mutable permutation to an immutable one
        -- and the action on the immutable
        a <- liftM f (unsafeFreeze p)

        -- Set the permutation to be equal to its successor
        hasNext <- setNext p
        
        -- If a successor exists, recurse, otherwise stop
        as <- unsafeInterleaveST $
            if hasNext then runOnSuccessors p 
                       else return []
        return (a:as)


forAllPermutesM_ :: (MPermute p m) => Int -> (Permute -> m a) -> m () 
forAllPermutesM_ n f = sequence_ $ forAllPermutes n f
{-# INLINE forAllPermutesM_ #-}

-- | Count the number of permutations of a given order
countAllPermutes :: Int -> Int
countAllPermutes n = length $ forAllPermutes n id

-- | Another version of the same function.  This one is slightly slower.
countAllPermutes2 :: Int -> Int
countAllPermutes2 n = runST $ do
    count <- newSTRef 0
    forAllPermutesM_ n $ (const $ modifySTRef' count (+1))
    readSTRef count
  where
    modifySTRef' var f = do
        old <- readSTRef var
        writeSTRef var $! f old

-- | Yet another version, this time using 'permutations' from Data.List.
-- This version is faster but uses more memory.
countAllPermutes3 :: Int -> Int
countAllPermutes3 n = length $ permutations [0 .. n-1]


-- | Print all permutations of a given order.
printAllPermutes :: Int -> IO ()
printAllPermutes n =
    forAllPermutesM_ n (putStrLn . show . elems)


main = do
    n  <- fmap (read . head) getArgs
    let count = countAllPermutes n
    putStrLn $ 
        "There are " ++ show count ++ " permutations of order " ++ show n ++ "."
    
