import Control.Exception
import System.Console.CmdArgs (cmdArgs)
import Control.Concurrent
import System.IO
import System.Directory
import System.FilePath

import ParseArgs 
import MainDriver
import Utilities 
import Find
import Strings
import Links

type URL = String

main :: IO ()
main = do
    args <- cmdArgs r34
    if null $ search args
        then maybeDL args
        else do
            eitherResult <- find (search args)
            either putStrLn (mapM_ putStrLn) eitherResult

askURL :: String -> IO (Either String URL)
askURL tag'
    | null tag' = askURL'
    | null $ scrub tag' = return $ Left invalidTag
    | otherwise = return . Right $ addBaseAddress (scrub tag')

askURL' :: IO (Either String URL)
askURL' = do
    putStr "Enter the tag which you wish to download: "
    hFlush stdout
    input <- getLine
    if null input
        then return $ Left emptyInput
        else if null $ scrub input
            then return $ Left invalidTag
            else return . Right $ addBaseAddress (scrub input)

getDir :: FilePath -> IO FilePath
getDir dir = do
    isDir <- doesDirectoryExist dir
    if isDir
        then return $ a dir
        else a <$> getCurrentDirectory
    where a = addTrailingPathSeparator

maybeDL :: R34 -> IO ()
maybeDL args = do
    eitherURL <- askURL (tag args)
    case eitherURL of
        Left err -> putStrLn err
        Right url -> do
            dir <- getDir (directory args)
            firstpage <- try $ openURL url :: IO (Either SomeException String)
            case firstpage of
                Left _ -> putStrLn noInternet
                Right val -> if noImagesExist val
                    then putStrLn $ noImages url
                    else do
                        imageLinks <- getImageLinks url putStrLn
                        if disableasync args
                            then niceDownload dir imageLinks putStrLn 
                                 =<< newEmptyMVar
                            else niceDownloadAsync dir imageLinks putStrLn
                                 =<< newMVar []
