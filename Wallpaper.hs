import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Char
import Control.Exception.Base
import System.Process

wpDir = "/home/andrfla/Pictures/InterfaceliftTest"
filePath = "FileFormat"

favPrefix       = "fav"
recentPrefix    = "recent"
dirPrefix       = "dir"

argumentError = "Invalid argument! Type 'wallpaper usage' for examples"
usageText = "TODO usage"

type Command = String
type Args = [String]

main = do
        ca <- getArgs
        case ca of  (command:args) -> execute command args 
                    [] -> set ["--random"]

execute :: Command -> Args -> IO ()
execute "add"       = add
execute "list"      = list
execute "set"       = set
execute "clear"     = clear
execute "usage"     = printUsage
execute _           = error argumentError

-- Implementations
list :: Args -> IO ()
list ["-a"] = list ["--all"]
list ["--all"] = do
                        entries <- getDirectoryContents wpDir
                        let wallpapers = unlines $ filter (not . (isPrefixOf ".")) entries
                        putStr wallpapers 
list ["-f"] = list ["--favorite"]
list ["--favorite"] = do
                        content <- getContent
                        let favorites = filterContent favPrefix content 
                            numberedFavorites = zipWith (\n f -> show n ++ " - " ++ f) [0..] favorites
                        putStr $ "Favorites:\n" ++ (unlines numberedFavorites)
list ["-r"] = list ["--recent"]
list ["--recent"] = do
                        content <- getContent
                        let recent = filterContent recentPrefix content 
                            numberedRecent = zipWith (\n f -> show n ++ " - " ++ f) [0..] recent
                        putStr $ "Recent:\n" ++ (unlines numberedRecent)
list ["-d"] = list ["--dir"]
list ["--dir"] = do
                        content <- getContent
                        let dir = filterContent dirPrefix content 
                            numberedDir = zipWith (\n f -> show n ++ " - " ++ f) [0..] dir
                        putStr $ "Directories:\n" ++ (unlines numberedDir)
list [] = do list ["-f"] ; list["-r"]
list _ = error argumentError
                        

add :: Args -> IO ()
add ["-d", path] = add ["--dir", path]
add ["--dir", path] | validPath (read path :: String) = do 
                                                    content <- getContent
                                                    let newDir = dirPrefix ++ " " ++ (read path :: String)
                                                        newContent = sortedContent $ content ++ "\n" ++ newDir
                                                    setContent newContent
add ["-f"] = add ["--favorite"]
add ["--favorite"] = add ["-f", "0"]
add ["-f", index] = add ["--favorite", index]
add ["--favorite", index] | all isNumber index = do 
                                            content <- getContent
                                            let newFavorite = favPrefix ++ " " ++ (filterContent recentPrefix content !! (read index :: Int))
                                                newContent = sortedContent $ content ++ "\n" ++ newFavorite
                                            setContent newContent
add _ = error argumentError

set :: Args -> IO ()
set ["--random"] = do
                    content <- getContent
                    let dirs = filterContent dirPrefix content
                    print dirs
                     
set _ = error argumentError

setBg :: String -> IO ProcessHandle
setBg filename = spawn $ "feh --bg-scale " ++ filename

clear :: Args -> IO ()
clear ["-f"] = clear ["--favorite"]
clear ["--favorite"] = do
                        content <- getContent
                        let newContent = clearContent favPrefix
                        setContent $ newContent content
clear ["-f", index] = clear ["--favorite", index]
clear ["--favorite", index] | all isNumber index = do
                                content <- getContent
                                let favorites = filterContent favPrefix content
                                    filteredFavorites = unlines . map (\a -> favPrefix ++ " " ++ a)
                                                                $ delete (favorites !! (read index :: Int)) favorites
                                    filteredContent = sortedContent $ filteredFavorites ++ 
                                                                    (unlines . filter (not . isPrefixOf favPrefix) $ lines content)
                                setContent filteredContent
clear ["-r"] = clear ["--recent"]
clear ["--recent"] = do
                        content <- getContent
                        let newContent = clearContent recentPrefix
                        setContent $ newContent content
clear ["-d"] = clear ["--dir"]
clear ["--dir"] = do
                    content <- getContent
                    let newContent = clearContent dirPrefix
                    setContent $ newContent content
clear ["-a"] = clear ["--all"] 
clear ["--all"] = do clear ["-r"]; clear ["-f"]; clear ["-d"]
clear _ = error argumentError

printUsage :: Args -> IO ()
printUsage _ = putStrLn usageText

-- Utilities
validPath :: String -> Bool
validPath s = True

sortedContent :: String -> String
sortedContent = unlines . sort . lines

filterContent :: String -> String -> [String]
filterContent s content = (map removePrefix) . (filter (s `isPrefixOf`)) $ lines content

clearContent :: String -> String -> String
clearContent prefix content = unlines . sort $ filter (not . isPrefixOf prefix) $ lines content

removePrefix :: String -> String
removePrefix = dropWhile isSpace . dropWhile (not . isSpace)

getContent :: IO (String)
getContent = readFile filePath

spawn :: String -> IO ProcessHandle
spawn s = runCommand s

setContent :: String -> IO ()
setContent content = bracketOnError (openTempFile "." "wallpaperTemp")
                                    (\(path, handle) -> do
                                        hClose handle
                                        removeFile path
                                        putStrLn "Error setting content!")
                                    (\(path, handle) -> do
                                        hPutStr handle content
                                        hClose handle
                                        removeFile filePath
                                        renameFile path filePath)
