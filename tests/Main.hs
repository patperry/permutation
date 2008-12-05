
import Control.Monad
import System.Environment
import Text.Printf

import Driver
import Pure
import ST

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    
    (results, passed) <- liftM unzip $
        foldM ( \prev (name,subtests) -> do
                     printf "\n%s\n" name
                     printf "%s\n" $ replicate (length name) '-'
                     cur <- mapM (\(s,a) -> printf "%-30s: " s >> a n) subtests
                     return (prev ++ cur)
              )
              []
              tests
               
    printf "\nPassed %d tests!\n\n" (sum passed)
    when (not . and $ results) $ fail "\nNot all tests passed!"
 where

    tests = [ ("Permute"   , tests_Permute)
            , ("STPermute" , tests_STPermute)
            ]
