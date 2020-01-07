module Main where

import Control.Monad
import Control.Exception
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import System.Process

import qualified Data.Text.IO as T

import Latte.Latte
import Latte.Backend.Stdlib

main :: IO ()
main = do
  args <- getArgs
  case args of
    file:_ -> do
      contents <- T.readFile file
      case buildX86 file contents of
        Right s -> do
          let asmName = file -<.> "s"
              binName = dropExtension file
          std <- putStdLib
          writeFile asmName (s <> "\n")
          callProcess "gcc" [asmName, std, "-m32", "-o", binName]
        Left e -> hPutStrLn stderr e >> exitFailure
    _ -> hPutStrLn stderr "wtf dude" >> exitFailure
