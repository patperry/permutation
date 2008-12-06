module Main where
    
import Control.Monad
import Control.Monad.ST
import Data.STRef
import System.Environment

import Data.Permute
import Data.Permute.MPermute
    
-- | Execute an action on every permutation of a given order.
{-# INLINE forAllPermutes #-}
forAllPermutes :: (MPermute p m) => Int -> (Permute -> m ()) -> m ()
forAllPermutes n a = do
    -- Allocate a mutable permutation initialized to the identity
    p  <- newPermute n
    
    -- Run the action on all successors of p
    runOnSuccessors p
    
  where
    runOnSuccessors p = do
        -- Cast the mutable permutation to an immutable one
        p' <- unsafeFreeze p  

        -- Run the action on the immutable permutation
        a p'

        -- Set the permutation to be equal to its successor
        hasNext <- setNext p
        
        -- If a successor exists, recurse, otherwise stop
        when hasNext $ runOnSuccessors p


        
        
-- | Count the number of permutations of a given order
countAllPermutes :: Int -> Int
countAllPermutes n = runST $ do
    count <- newSTRef 0
    forAllPermutes n (const $ modifySTRef count (+1))
    readSTRef count

        
-- | Print all permutations of a given order.
printAllPermutes :: Int -> IO ()
printAllPermutes n =
    forAllPermutes n (putStrLn . show . elems)
    

main = do
    n  <- fmap (read . head)getArgs
    let count = countAllPermutes n
    putStrLn $ 
        "There are " ++ show count ++ " permutations of order " ++ show n ++ "."
    
