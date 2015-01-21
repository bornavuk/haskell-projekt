-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy

module Commands
(commands)
where

import Exec
import Data.Char (ord)
import Numeric (showHex)
import qualified Data.Map as M
import System.Directory
--import System.Path (copyDir)
import Control.Monad (liftM, filterM, forM_, mapM_, when)
import System.FilePath ((</>), addTrailingPathSeparator)
import Data.List ((\\))

commands :: M.Map String Command
commands = foldr (\(name, cmd) -> M.insert name cmd) M.empty [
    ("pwd", pwd), ("echo", echo), ("ls", ls), ("cd", cd),
    ("mv", mv), ("cpdir", cpdir), ("mkdir", mkdir), ("rmdir", rmdir),
    ("cp", cp), ("rm", rm), ("create", create),
    ("hexdump", hexdump)
    ]

echo :: Command
echo args ss = do
    return $ ScriptState (unwords args) (wd ss) (vartable ss)

pwd :: Command
pwd _ ss = do
    return $ ScriptState (wd ss) (wd ss) (vartable ss)

ls :: Command
ls [] (ScriptState _ wd vartbl) = do
    cts <- getDirectoryContents wd
    return $ ScriptState (unlines cts) wd vartbl
ls [dir] (ScriptState _ wd vartbl) = do
    cts <- getDirectoryContents dir
    return $ ScriptState (unlines cts) wd vartbl
ls _ (ScriptState _ wd vartbl) = do
    putStr "prevelik broj argumenata"
    return $ ScriptState "" wd vartbl

cd :: Command
cd [] (ScriptState _ _ vartbl) = do
    wd <- getHomeDirectory
    setCurrentDirectory wd
    return $ ScriptState ("radni direktorij: " ++ wd) wd vartbl
cd [dir] (ScriptState _ wd vartbl) = do
    setCurrentDirectory dir
    wd <- getCurrentDirectory
    return $ ScriptState ("radni direktorij: " ++ wd) wd vartbl
cd _ (ScriptState _ wd vartbl) = do
    putStr "prevelik broj argumenata"
    return $ ScriptState "" wd vartbl

mv :: Command
mv args ss@(ScriptState _ wd vartbl) = do
    if length args < 2 then do
        putStr "potrebno dati barem dva argumenta"
    else do
        firstIsFile <- doesFileExist $ head args
        firstIsDir <- doesDirectoryExist $ head args
        lastIsFile <- doesFileExist $ last args
        lastIsDir <- doesDirectoryExist $ last args
        mv' ss firstIsFile firstIsDir lastIsFile lastIsDir (init args) $ last args
    return $ ScriptState "" wd vartbl
--mv' :: ss fif fid lif lid args target
-- moving file in another file
mv' _ True _ _ False [src] trgt = do
    copyFile src trgt
    removeFile src
    putStr "datoteka uspjesno premjestena"
mv' _ True _ _ False _ _ = putStr "prevelik broj argumenata"
-- moving file(s) in another directory
mv' _ True _ _ True [arg] trgt = do
    copyFile arg (trgt ++ "\\" ++ getDirName arg)
    removeFile arg
    putStr "premjestanje uspjelo"
mv' ss True _ _ True (arg:args) trgt = do
    copyFile arg (trgt ++ "\\" ++ getDirName arg)
    removeFile arg
    mv (reverse (trgt : reverse args)) ss
    return ()
-- moving directories in another directory
mv' _ _ True _ True [arg] trgt = do
    copyDir arg (trgt ++ "\\" ++ getDirName arg)
    removeDirectoryRecursive arg
    putStr "premjestanje uspjelo"
mv' ss _ True _ True (arg:args) trgt = do
    copyDir arg (trgt ++ "\\" ++ getDirName arg)
    removeDirectoryRecursive arg
    mv (reverse (trgt : reverse args)) ss
    return ()
-- renaming directory
mv' ss _ True False False [src] trgt = do
    renameDirectory src trgt
    putStr "direktorij uspjesno preimenovan"
mv' _ _ True False False _ _ = do
    putStr "prevelik broj argumenata"
-- wrong call
mv' _ _ _ _ _ _ _ = putStr "zadana datoteka/direktorij ne postoji, naredba prekinuta"

getDirName :: String -> String
getDirName = reverse . takeWhile (/= '\\') . reverse

cpdir :: Command
cpdir args (ScriptState _ wd vartbl)
    | length args < 2 = do 
        putStr "potrebno dati barem dva argumenta"
        return $ ScriptState "" wd vartbl
    | otherwise = do 
        createDirectoryIfMissing True $ last args
        cpdir' (last args) $ init args
        putStr "kopiranje direktorija uspjelo"
        return $ ScriptState "" wd vartbl
cpdir' target [arg] = do
    copyDir arg $ target ++ "\\" ++ getDirName arg
cpdir' target (arg:args) = do
    copyDir arg $ target ++ "\\" ++ getDirName arg
    cpdir' target args

mkdir :: Command --todo obavijest da vec postoji
mkdir [] (ScriptState _ wd vartbl) = do
    putStr "potrebno dati barem jedan argument"
    return $ ScriptState "" wd vartbl
mkdir [arg] (ScriptState _ wd vartbl) = do
    createDirectoryIfMissing True arg
    putStr "direktoriji uspjesno napravljeni"
    return $ ScriptState "" wd vartbl
mkdir (arg:args) ss = do
    createDirectoryIfMissing True arg
    mkdir args ss

rmdir :: Command --todo ukloniti samo prazne, sad uklanja sve
rmdir [] (ScriptState _ wd vartbl) = do
    putStr "potrebno dati barem jedan argument"
    return $ ScriptState "" wd vartbl
rmdir [arg] (ScriptState _ wd vartbl) = do
    removeDirectoryRecursive arg
    putStr "direktoriji uspjesno uklonjeni"
    return $ ScriptState "" wd vartbl
rmdir (arg:args) ss = do
    removeDirectoryRecursive arg
    rmdir args ss

cp :: Command
cp args (ScriptState _ wd vartbl)
    | length args < 2 = do 
        putStr "potrebno dati barem dva argumenta"
        return $ ScriptState "" wd vartbl
    | otherwise = do
        lastIsDir <- doesDirectoryExist $ last args
        if lastIsDir then do
            cp' (last args) $ init args
            putStr "kopiranje datoteka uspjelo"
        else
            if length args > 2 then
                putStr "prevelik broj argumenata"
            else do
                copyFile (args!!0) (args!!1)
                putStr "kopiranje datoteka uspjelo"
        return $ ScriptState "" wd vartbl
cp' target [arg] = do
    copyFile arg (target ++ "\\" ++ getDirName arg)
cp' target (arg:args) = do
    copyFile arg (target ++ "\\" ++ getDirName arg)
    cp' target args

rm :: Command
rm [] (ScriptState _ wd vartbl) = do
    putStr "potrebno dati barem jedan argument"
    return $ ScriptState "" wd vartbl
rm [arg] (ScriptState _ wd vartbl) = do
    f <- doesFileExist arg
    when f $ removeFile arg
    putStr "postojece datoteke uspjesno uklonjene"
    return $ ScriptState "" wd vartbl
rm (arg:args) ss = do
    f <- doesFileExist arg
    when f $ removeFile arg
    rm args ss

create :: Command
create [] (ScriptState _ wd vartbl) = do
    putStr "potrebno dati barem jedan argument"
    return $ ScriptState "" wd vartbl
create [arg] (ScriptState _ wd vartbl) = do
    f <- doesFileExist arg
    when (not f) $ writeFile arg ""
    putStr "nepostojece datoteke uspjesno napravljene"
    return $ ScriptState "" wd vartbl
create (arg:args) ss = do
    f <- doesFileExist arg
    when (not f) $ writeFile arg ""
    create args ss

hexdump :: Command
hexdump [arg] (ScriptState _ wd vartbl) = do
    e <- doesFileExist arg
    if e then do
        f <- readFile arg
        return $ ScriptState (toHexDump f "") wd vartbl
    else do
        putStr "nepostojeca datoteka"
        return $ ScriptState "" wd vartbl
hexdump _ (ScriptState _ wd vartbl) = do
    putStr "potrebno dati tocno jedan argument"
    return $ ScriptState "" wd vartbl
toHexDump :: String -> String -> String
toHexDump (x:xs) ys = toHexDump xs (ys ++ toHex x)
toHexDump [] ys = ys
toHex :: Char -> String
toHex c = (if length h == 1 then "0" else "") ++ h
    where h = showHex (ord c) ""

{-cat :: Command
cat [] (ScriptState _ wd vartbl) = do
    return $ ScriptState "" wd vartbl
cat [arg] (ScriptState out wd vartbl) = do
    e <- doesFileExist arg
    if e then do
        f <- readFile arg
        return $ ScriptState f wd vartbl
    else do
        putStr "nepostojeca datoteka"
        return $ ScriptState "" wd vartbl
cat (arg:args) (ScriptState _ wd vartbl) = do
    e <- doesFileExist arg
    if e then do
        f <- readFile arg
        return $ ScriptState () wd vartbl
    else do
        putStr "nepostojeca datoteka"
        return $ ScriptState "" wd vartbl-}



------------------------------------------------
------------------------------------------------
-- module System.Path that couldn't be installed

-- | Remove useless paths from a list of paths.
filterUseless :: [FilePath] -> [FilePath]
filterUseless = (\\ [".", ".."])

-- | Returns a list of nodes in a tree via a depth-first walk.
mtreeList :: Monad m => (a -> m [a]) -> a -> m [a]
mtreeList children root = do
  xs <- children root
  subChildren <- mapM (mtreeList children) xs
  return $ root : concat subChildren

-- | Get a list of files in path, but not recursively. Removes '.' and '..'.
topFileList :: FilePath -> IO [FilePath]
topFileList path =
  fmap (map (path </>) . filterUseless) $ getDirectoryContents path

-- | Recursively list the contents of a directory. Depth-first.
fileList :: FilePath -> IO [FilePath]
fileList = mtreeList children
  where children path = do
          directory <- doesDirectoryExist path
          if directory
            then topFileList path
            else return []

-- | We can use this data type to represent the pieces of a directory.
data Directory = Directory
                 { -- | The path of the directory itself.
                   dirPath :: FilePath
                   -- | All subdirectories of this directory.
                 , subDirs :: [FilePath]
                   -- | All files contained in this directory.
                 , files   :: [FilePath]
                 }
               deriving (Show)

-- | Creates a Directory instance from a FilePath.
createDir :: FilePath -> IO Directory
createDir path = do
  contents <- topFileList path
  subdirs  <- filterM doesDirectoryExist contents
  files    <- filterM doesFileExist contents
  return (Directory path subdirs files)
  
-- | Walk a directory depth-first. Similar to Python's os.walk and fs.core/walk
-- from the fs Clojure library.
walkDir :: FilePath -> IO [Directory]
walkDir root = createDir root >>= mtreeList children
  where children path = do
          let dirs = subDirs path
          mapM createDir dirs

-- | Given a root (prefix), remove it from a path. This is useful
-- for getting the filename and subdirs of a path inside of a root.
removeRoot :: FilePath -> FilePath -> FilePath
removeRoot prefix = drop . length $ addTrailingPathSeparator prefix

-- | Given a root path, a new root path, and a path to be changed,
-- removes the old root from the path and replaces it with to.
replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot root to path = to </> removeRoot root path

-- | Copy a directory recursively. Moves every file, creates every directory.
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing True to
  walked <- walkDir from
  forM_ walked $ \(Directory _ dirs files) -> do
    mapM_ (createDirectoryIfMissing True . replaceRoot from to) dirs
    forM_ files $ \path -> copyFile path (replaceRoot from to path)