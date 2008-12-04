#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Cmd
> import System.Exit ( ExitCode(..) )
>
> testing _ _ _ _ = do
>     err <- system "make -C tests"
>     system "make -s -C tests clean"
>     if err /= ExitSuccess
>         then ioError $ userError $ "failed"
>         else return ()
>
> main = defaultMainWithHooks simpleUserHooks
>        {runTests=testing}
