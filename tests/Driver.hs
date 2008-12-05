-----------------------------------------------------------------------------
-- |
-- Module     : Driver
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Driver (
    Natural(..),
    Index(..),
    ListPermute(..),
    InvSwapsPermute(..),
    Swap(..),
    
    mytest,
    mycheck,
    mytests,
    done,

    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.ST

import Data.AEq
import Data.Complex
import Data.Ix
import Data.Function
import Data.List
import Data.Ord

import Debug.Trace

import System.IO
import System.Random

import Test.QuickCheck

import Text.Printf
import Text.Show.Functions

newtype Natural = Nat Int deriving (Eq,Show)
instance Arbitrary Natural where
    arbitrary = do
        n <- arbitrary
        return $ Nat (abs n)
    
    coarbitrary = undefined

data Index = Index Int Int deriving (Eq,Show)
instance Arbitrary Index where
    arbitrary = do
        n <- arbitrary
        i <- choose (0, abs n)
        return $ Index (abs n + 1) i

    coarbitrary = undefined

data ListPermute = ListPermute Int [Int] deriving (Eq,Show)
instance Arbitrary ListPermute where
    arbitrary = do
        (Nat n) <- arbitrary
        xs <- vector n :: Gen [Int]
        return . ListPermute n $ 
            (snd . unzip) $ sortBy (comparing fst) $ zip xs [0..]

    coarbitrary = undefined

data InvSwapsPermute = InvSwapsPermute Int [(Int,Int)] deriving (Eq,Show)
instance Arbitrary InvSwapsPermute where
    arbitrary = do
        (Nat n) <- arbitrary
        let n' = n + 1
        (Nat k) <- arbitrary
        ss <- replicateM k (swap n')
        return $ InvSwapsPermute n' ss

    coarbitrary = undefined

swap n = do
    i <- choose (0,n-1)
    j <- choose (0,n-1)
    return (i,j)


data Swap = Swap Int Int Int deriving (Eq,Show)
instance Arbitrary Swap where
    arbitrary = do
        (Index n i) <- arbitrary
        j <- choose (0,n-1)
        return $ Swap n i j

    coarbitrary = undefined

instance Arbitrary Ordering where
    arbitrary   = elements [ LT, GT, EQ ]
    coarbitrary = coarbitrary . fromEnum

------------------------------------------------------------------------
--
-- QC driver ( taken from xmonad-0.6 )
--

debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

