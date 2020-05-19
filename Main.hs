module Main where

import Parser
import ParserBase

import Control.Monad
-- rename monad return for easy use!
mreturn = Control.Monad.return

main :: IO ()
main = mreturn ()
