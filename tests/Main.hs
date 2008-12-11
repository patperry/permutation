
import Control.Monad
import System.Environment
import Text.Printf

import Driver
import Choose
import Permute
import STChoose
import STPermute

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    
    (smokeResults, smokePassed) <- liftM unzip $
        foldM ( \prev (name,subtests) -> do
                    printf "\n%s\n" name
                    printf "%s\n" $ replicate (length name) '-'
                    cur <- mapM (\(s,a) -> printf "%-30s: " s >> a 1) subtests
                    return (prev ++ cur)
              )
              []
              smoke

    (results, passed) <- liftM unzip $
        foldM ( \prev (name,subtests) -> do
                     printf "\n%s\n" name
                     printf "%s\n" $ replicate (length name) '-'
                     cur <- mapM (\(s,a) -> printf "%-30s: " s >> a n) subtests
                     return (prev ++ cur)
              )
              []
              tests
               
    printf "\nPassed %d tests!\n\n" (sum $ smokePassed ++ passed)
    when (not . and $ smokeResults ++ results) $ fail "\nNot all tests passed!"
 where

    smoke = [ ("STChoose" , smoke_STChoose) 
            , ("STPermute", smoke_STPermute) 
            ]

    tests = [ ("STChoose"  , tests_Choose)
            , ("STChoose"  , tests_STChoose)
            , ("Permute"   , tests_Permute)
            , ("STPermute" , tests_STPermute)
            ]
