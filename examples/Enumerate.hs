{-# LANGUAGE BangPatterns, Rank2Types #-}
module Main where
    
import Control.Monad
import Control.Monad.ST
import Data.List( permutations )
import Data.STRef
import System.Environment

import Data.Permute
import Data.Permute.MPermute
    
-- | Execute an action on every permutation of a given order.
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
{-# INLINE forAllPermutes #-}

{-# INLINE forAllPermutesM #-}
forAllPermutesM :: (MPermute p m) => Int -> (Permute -> m a) -> m [a]
forAllPermutesM n f = sequence $ forAllPermutes n f

{-# INLINE forAllPermutesM_ #-}
forAllPermutesM_ :: (MPermute p m) => Int -> (Permute -> m a) -> m () 
forAllPermutesM_ n f = sequence_ $ forAllPermutes n f

-- | Count the number of permutations of a given order
countAllPermutes :: Int -> Int
countAllPermutes n = length $ forAllPermutes n id


countAllPermutes2 :: Int -> Int
countAllPermutes2 n = runST $ do
    count <- newSTRef 0
    forAllPermutesM_ n $ (const $ modifySTRef' count (+1))
    readSTRef count
  where
    modifySTRef' var f = do
        old <- readSTRef var
        writeSTRef var $! f old

countAllPermutes3 :: Int -> Int
countAllPermutes3 n = length $ permutations [1..n]
        
-- | Print all permutations of a given order.
printAllPermutes :: Int -> IO ()
printAllPermutes n =
    forAllPermutesM_ n (putStrLn . show . elems)

printAllPermutes2 :: Int -> IO ()
printAllPermutes2 n =
    sequence_ [ (putStrLn . show) p | p <- permutations [ 0 .. n-1 ] ]
    -- forAllPermutesM_ n (putStrLn . show . elems)
    

main = do
    n  <- fmap (read . head) getArgs
    let count = countAllPermutes n
    putStrLn $ 
        "There are " ++ show count ++ " permutations of order " ++ show n ++ "."
    
