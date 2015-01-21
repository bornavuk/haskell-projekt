-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash ( runInteractive, runScript )
where

import Commands
import Exec
import HashParser
import System.Directory

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
    putStr "hash> "
    input <- getLine
    if take 4 input == "exit" then return () else do
        filepath <- getCurrentDirectory
        ss <- runHashProgram commands (Left filepath) (parsiraj input)
        continueInteractive ss
        return ()

-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript f = do
    skripta <- readFile f
    filepath <- getCurrentDirectory
    runHashProgram commands (Left filepath) $ parsiraj.init.unlines.filter (not.null).map (takeWhile (/='#')) $ lines skripta
    putStrLn "skripta izvrsena"

-- continues interactive with saved script state
continueInteractive :: ScriptState -> IO ScriptState
continueInteractive ss = do
    putStr "hash> "
    input <- getLine
    if take 4 input == "exit" then return ss else do
        ss' <- runHashProgram commands (Right ss) (parsiraj input)
        continueInteractive ss'
    


