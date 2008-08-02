
import Control.Monad ( forM_ )
import Data.List ( sortBy )
import Data.Ord  ( comparing )
import Foreign
import System.Random
import Test.HUnit

import Data.Permutation



swapElems :: Storable a => Ptr a -> Int -> Int -> IO ()
swapElems ptr i j =  do
    x <- peekElemOff ptr i
    y <- peekElemOff ptr j
    pokeElemOff ptr i y
    pokeElemOff ptr j x

newRandomArray :: (Random a, Storable a) => Int -> IO (Ptr a)
newRandomArray n = do
    xs <- sequence $ replicate n randomIO
    newArray xs

newArrayCopy :: Storable a => Int -> Ptr a -> IO (Ptr a)
newArrayCopy n x = do
    y <- mallocArray n
    copyArray y x n
    return y

newRandomPerm :: Int -> IO (Permutation)
newRandomPerm n = do
    xs <- sequence $ replicate n randomIO :: IO [Int]
    let ixs  = zip [0..] xs
        ixs' = sortBy (comparing snd) ixs
        is   = (fst . unzip) ixs'
        p    = permutation n is
    return p
    

testIdentity :: Test
testIdentity = TestCase $ do
    let n = 30
        p = identity n
    
    x <- newRandomArray n :: IO (Ptr Double)
    y <- newArrayCopy n x
    applyWith (swapElems y) p
    
    xs <- peekArray n x
    ys <- peekArray n y
    
    assertEqual ""
        xs
        ys
        
        
testSmallCycle :: Test
testSmallCycle = TestCase $ do
    let n = 3
        p = permutation 3  [ 2, 0, 1 ]

    x <- newArray [ 6, 7, 8 ] :: IO (Ptr Int)
    applyWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ 8, 6, 7 ]        
        xs

testSmallCycleInv :: Test
testSmallCycleInv = TestCase $ do
    let n = 3
        p = permutation 3  [ 2, 0, 1 ]

    x <- newArray [ 6, 7, 8 ] :: IO (Ptr Int)
    invertWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ 7, 8, 6 ]
        xs


testSingleCycleInv :: Test
testSingleCycleInv = TestCase $ do
    let n = 5
        p = permutation n [ 3, 0, 1, 4, 2 ]
        
    x <- newArray [ 0.237,  0.382,  0.818, -0.413,  0.037 ] :: IO (Ptr Double)
    invertWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ 0.382, 0.818, 0.037, 0.237, -0.413 ]
        xs

testSingleCycle :: Test
testSingleCycle = TestCase $ do
    let n = 5
        p = permutation n [ 3, 0, 1, 4, 2 ]

    x <- newArray [ 0.237,  0.382,  0.818, -0.413,  0.037 ] :: IO (Ptr Double)
    applyWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ -0.413, 0.237, 0.382, 0.037, 0.818 ]
        xs
        
testMultiCycleInv :: Test
testMultiCycleInv = TestCase $ do
    let n = 5
        p = permutation n [ 2, 3, 0, 4, 1 ]
        
    x <- newArray [ 0.237,  0.382,  0.818, -0.413,  0.037 ] :: IO (Ptr Double)
    invertWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ 0.818, 0.037, 0.237, 0.382, -0.413 ]
        xs

testMultiCycle :: Test
testMultiCycle = TestCase $ do
    let n = 5
        p = permutation n [ 2, 3, 0, 4, 1 ]
    x <- newArray [ 0.237,  0.382,  0.818, -0.413,  0.037 ] :: IO (Ptr Double)
    applyWith (swapElems x) p
    
    xs <- peekArray n x
    assertEqual ""
        [ 0.818, -0.413, 0.237, 0.037, 0.382 ]
        xs

testApply :: Int -> Test
testApply n = TestCase $ do
    x <- newRandomArray n :: IO (Ptr Double)
    y <- newArrayCopy n x
    p <- newRandomPerm n
    applyWith  (swapElems y) p
    
    xs <- peekArray n x
    ys <- peekArray n y

    forM_ [0..(n-1)] $ \i ->
        assertEqual ("position " ++ show i) (xs !! (apply p i)) (ys !! i)

testInvert :: Int -> Test
testInvert n = TestCase $ do
    x <- newRandomArray n :: IO (Ptr Double)
    y <- newArrayCopy n x
    p <- newRandomPerm n
    invertWith  (swapElems y) p
    
    xs <- peekArray n x
    ys <- peekArray n y

    forM_ [0..(n-1)] $ \i ->
        assertEqual ("position " ++ show i) (xs !! i) (ys !! (apply p i))


testApplyThenInv :: Int -> Test
testApplyThenInv n = TestCase $ do
    x <- newRandomArray n :: IO (Ptr Double)
    y <- newArrayCopy n x
    p <- newRandomPerm n
    applyWith  (swapElems y) p
    invertWith (swapElems y) p
    
    xs <- peekArray n x
    ys <- peekArray n y
    
    assertEqual "" xs ys

testInvThenApply :: Int -> Test
testInvThenApply n = TestCase $ do
    x <- newRandomArray n :: IO (Ptr Double)
    y <- newArrayCopy n x
    p <- newRandomPerm n
    invertWith (swapElems y) p
    applyWith  (swapElems y) p
    
    xs <- peekArray n x
    ys <- peekArray n y
    
    assertEqual "" xs ys



 
smallTests = [ TestLabel "identity"             testIdentity
             , TestLabel "small cycle"          testSmallCycle
             , TestLabel "small cycle inverse"  testSmallCycleInv
             , TestLabel "single cycle"         testSingleCycle
             , TestLabel "single cycle inverse" testSingleCycleInv
             , TestLabel "multi cycle"          testMultiCycle
             , TestLabel "multi cycle inverse"  testMultiCycleInv
             ]

 
main =
    let ns      = [ 0, 1, 2, 3, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048 ]
        aTests  = map (\n -> TestLabel ("apply (" ++ show n ++ ")") 
                                       (testApply n)) ns
        iTests  = map (\n -> TestLabel ("apply (" ++ show n ++ ")") 
                                       (testInvert n)) ns
        aiTests = map (\n -> TestLabel ("apply then inverse (" ++ show n ++ ")") 
                                       (testApplyThenInv n)) ns
        iaTests = map (\n -> TestLabel ("inverse then apply (" ++ show n ++ ")") 
                                       (testInvThenApply n)) ns
        tests   = TestList $ smallTests ++ aTests ++ aiTests ++ iaTests
    in runTestTT tests
    