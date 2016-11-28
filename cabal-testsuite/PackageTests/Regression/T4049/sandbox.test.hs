import Test.Cabal.Prelude
import Distribution.Simple.Program.Builtin
main = cabalTest $ do
    withSandbox $ do
        cabal "install" ["--enable-shared"]
        have_gcc <- isAvailableProgram gccProgram
        env <- getTestEnv
        let exe_name = testWorkDir env </> "UseLib"
            args = [ "UseLib.c"
                   , "-o", exe_name
                   , "-l", "myforeignlib"
                   , "-L", testSandboxDir env </> "lib"]
        -- Windows does not have GCC, and unfortunately
        -- ghc -no-hs-main doesn't seem to work correclty in this case.
        if have_gcc
            then runProgramM gccProgram args
            else runProgramM ghcProgram ("-no-hs-main" : args)
        cabal "exec" [exe_name]
