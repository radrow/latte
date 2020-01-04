{-# LANGUAGE TemplateHaskell #-}
module Latte.Backend.Stdlib where

import Data.FileEmbed

import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

stdlib :: BS.ByteString
stdlib = $(embedFile "stdlib/stdlib.c")

putStdLib :: IO FilePath
putStdLib = do
  tmp <- getTemporaryDirectory
  let file = tmp </> "latte_stdlib.c"
  BS.writeFile file stdlib
  return file
