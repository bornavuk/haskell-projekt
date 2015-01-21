--module Main where

import System.Environment
import Hash

main = do
    args <- getArgs
    if length args == 0 then runInteractive
    else runScript (args!!0)
