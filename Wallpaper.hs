import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Char
import Control.Exception.Base (bracketOnError)
import System.Process
import System.Random

wpDir = "/home/andrfla/Pictures/InterfaceliftTest"
filePath = "FileFormat"

favPrefix       = "fav"
recentPrefix    = "recent"
dirPrefix       = "dir"
maxRecent       = 10

argumentError = "Invalid argument! Try 'wallpaper --usage'"
usageText = "TODO" 

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
execute "--usage"   = printUsage
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
                        let favorites = map (removeDir . removePrefix) . lines $ filterContent favPrefix content 
                            numberedFavorites = zipWith (\n f -> show n ++ " - " ++ f) [0..] favorites
                        putStr $ "Favorites:\n" ++ (unlines numberedFavorites)
list ["-r"] = list ["--recent"]
list ["--recent"] = do
                        content <- getContent
                        let recent = map (removeDir . removePrefix) . lines $ filterContent recentPrefix content 
                            numberedRecent = zipWith (\n f -> show n ++ " - " ++ f) [0..] recent
                        putStr $ "Recent:\n" ++ (unlines numberedRecent)
list ["-d"] = list ["--dir"]
list ["--dir"] = do
                        content <- getContent
                        let dirs = map removePrefix . lines $ filterContent dirPrefix content 
                            numberedDir = zipWith (\n dir -> show n ++ " - " ++ dir) [0..] dirs
                        putStr $ "Directories:\n" ++ (unlines numberedDir)
list [] = do list ["-d"]; list ["-f"] ; list["-r"]
list _ = error argumentError
                        

add :: Args -> IO ()
add ["-d", path] = add ["--dir", path]
add ["--dir", path] | validPath path = do 
                                    content <- getContent
                                    let newDir = dirPrefix ++ " " ++ path
                                        newContent = sortedContent $ content ++ newDir
                                    setContent newContent
add ["-f"] = add ["--favorite"]
add ["--favorite"] = add ["-f", "0"]
add ["-f", index] = add ["--favorite", index]
add ["--favorite", index] | all isNumber index = do 
                                    content <- getContent
                                    let newFavorite = favPrefix ++ " " 
                                                ++ (removePrefix $ lines (filterContent recentPrefix content) !! (read index :: Int))
                                        newContent = sortedContent $ content ++ newFavorite
                                    setContent newContent
add _ = error argumentError

set :: Args -> IO ()
set ["--random"] = do
                    content <- getContent 
                    let dirs = map removePrefix . lines $ filterContent dirPrefix content
                    r <- mapM getDirectoryContents dirs
                    let files = filter (not . isSuffixOf ".") . concat $ zipWith (\dir l -> map ((dir ++ "/")++) l) dirs r
                    choice <- randomChoice files
                    setContent $ content `addRecent` choice
                    setBg choice
                    return ()
set ["-f"] = set ["--favorite"]
set ["--favorite"] = do
                    content <- getContent
                    let favorites = map removePrefix . lines $ filterContent favPrefix content
                    choice <- randomChoice favorites
                    setBg choice
                    return ()
set ["-f", index] = set ["--favorite", index]
set ["--favorite", index] | all isNumber index = do
                    content <- getContent
                    let favorites = map removePrefix . lines $ filterContent favPrefix content
                        choice = favorites !! (read index :: Int)
                    setBg choice
                    return ()
set ["-r", index] = set ["--recent", index]
set ["--recent", index] | all isNumber index = do
                    content <- getContent
                    let recent = map removePrefix . lines $ filterContent recentPrefix content
                        choice = recent !! (read index :: Int)
                    setBg choice
                    return ()
set _ = error argumentError
                    

clear :: Args -> IO ()
clear ["-f"] = clear ["--favorite"]
clear ["--favorite"] = do
                        content <- getContent
                        let newContent = clearContent favPrefix
                        setContent $ newContent content
clear ["-f", index] = clear ["--favorite", index]
clear ["--favorite", index] | all isNumber index = do
                        content <- getContent
                        let favorites = lines $ filterContent favPrefix content
                            filteredFavorites = unlines $ delete (favorites !! (read index :: Int)) favorites
                            filteredContent = sortedContent $ filteredFavorites ++ (clearContent favPrefix content)
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
clear ["--all"] = do clear ["-r"]; clear ["-f"]
clear _ = error argumentError

printUsage :: Args -> IO ()
printUsage _ = putStrLn usageText

-- Utilities
validPath :: String -> Bool
validPath s = True

randomChoice :: [a] -> IO a
randomChoice l = do
                    index <- getStdRandom (randomR (0, length l - 1))
                    return (l !! index)


sortedContent :: String -> String
sortedContent = unlines . (sortBy $ compare . getPrefix) . lines

filterContent :: String -> String -> String
filterContent s content = unlines . (filter (s `isPrefixOf`)) $ lines content

clearContent :: String -> String -> String
clearContent prefix content = sortedContent . unlines $ filter (not . isPrefixOf prefix) $ lines content

getPrefix :: String -> String
getPrefix = takeWhile (not . isSpace)

removePrefix :: String -> String
removePrefix = dropWhile isSpace . dropWhile (not . isSpace)

addPrefix :: String -> String -> String
addPrefix prefix name = prefix ++ " " ++ name ++ "\n"

removeDir :: String -> String
removeDir = reverse . takeWhile (not . ('/' ==)) . reverse

setBg :: String -> IO ProcessHandle
setBg filename = runCommand $ "feh --bg-scale " ++ filename

addRecent :: String -> String -> String
addRecent content filename =   unlines (contentNoRecent ++ newRecent)
                                where   contentNoRecent = lines $ clearContent recentPrefix content
                                        newRecent = (take maxRecent) . lines $ (addPrefix recentPrefix filename) ++ (filterContent recentPrefix content)

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
