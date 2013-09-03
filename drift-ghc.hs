import Data.List (isInfixOf)
import System.Cmd (rawSystem)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess))
import Paths_DrIFT (getBinDir)

main :: IO ExitCode
main = do args <- getArgs
          case args of
            (a:b:c:[]) -> conditional a b c
            _ -> error "This is a driver script allowing DrIFT to be used seamlessly with ghc.\n \
                       \ in order to use it, pass '-pgmF drift-ghc -F' to ghc when compiling your programs."

conditional ::  FilePath -> FilePath -> FilePath -> IO ExitCode
conditional orgnl inf outf = do prefix <- getBinDir
                                infile <- readFile inf
                                if "{-!" `isInfixOf` infile then do putStrLn (prefix ++ "DriFT-cabalized " ++
                                                                              inf ++ " -o " ++ outf)
                                                                    rawSystem inf ["-o", outf]
                                 else do writeFile outf ("{-# LINE 1 \"" ++ orgnl ++ " #-}")
                                         readFile inf >>= appendFile outf
                                         return ExitSuccess
{- GHC docs say: "-pgmF cmd
   Use cmd as the pre-processor (with -F only).
Use -pgmF cmd  to select the program to use as the preprocessor.
When invoked, the cmd pre-processor is given at least three arguments on its command-line:
1. the first argument is the name of the original source file,
2. the second is the name of the file holding the input
3. third is the name of the file where cmd should write its output to." -}