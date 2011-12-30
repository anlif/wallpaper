import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Char
import Control.Exception.Base

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
execute "favorite"  = favorite
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
list [] = do list ["-f"] ; list["-r"]
list _ = error argumentError
                        

favorite :: Args -> IO ()
favorite [] = favorite ["0"]
favorite [index] | all isNumber index = do 
                                content <- getContent
                                let newFavorite = favPrefix ++ " " ++ (filterContent recentPrefix content !! (read index :: Int))
                                    newContent = sortedContent $ content ++ "\n" ++ newFavorite
                                setContent newContent
favorite ["-d", index] = favorite ["--delete", index]
favorite ["--delete", index] | all isNumber index = do
                                content <- getContent
                                let favorites = filterContent favPrefix content
                                    filteredFavorites = unlines . map (\a -> favPrefix ++ " " ++ a)
                                                                $ delete (favorites !! (read index :: Int)) favorites
                                    filteredContent = sortedContent $ filteredFavorites ++ 
                                                                    (unlines . filter (not . isPrefixOf favPrefix) $ lines content)
                                setContent filteredContent
favorite _ = error argumentError

set :: Args -> IO ()
set _ = putStrLn "set Not implemented!"

clear :: Args -> IO ()
clear ["-f"] = clear ["--favorite"]
clear ["--favorite"] = do
                        content <- getContent
                        let newContent = sortedContent . unlines $ filter (not . isPrefixOf favPrefix) $ lines content
                        setContent newContent
clear ["-r"] = clear ["--recent"]
clear ["--recent"] = do
                        content <- getContent
                        let newContent = sortedContent . unlines $ filter (not . isPrefixOf recentPrefix) $ lines content
                        setContent newContent
clear ["-a"] = clear ["--all"] 
clear ["--all"] = do clear ["-r"]; clear ["-f"]
clear _ = error argumentError

printUsage :: Args -> IO ()
printUsage _ = putStrLn usageText

-- Utilities
sortedContent :: String -> String
sortedContent = unlines . sort . lines

filterContent :: String -> String -> [String]
filterContent s content = (map removePrefix) . (filter (s `isPrefixOf`)) $ lines content

removePrefix :: String -> String
removePrefix = dropWhile isSpace . dropWhile (not . isSpace)

getContent :: IO (String)
getContent = readFile filePath

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
