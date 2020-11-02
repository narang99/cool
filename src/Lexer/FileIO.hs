module Lexer.FileIO where

import System.IO
import Data.Text.Lazy.IO as TLIO
import Data.Text.Lazy as TL

readFile ::  Handle -> IO TL.Text
readFile = TLIO.hGetContents
