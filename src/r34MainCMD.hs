import Control.Exception (SomeException, try)
import System.Console.CmdArgs (cmdArgs)
import Data.Either (either)
import Control.Concurrent (newMVar)
import System.IO (hFlush, stdout)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (addTrailingPathSeparator)
import ParseArgs (R34(..), r34)
import MainDriver (noImagesExist, desiredSection, getPageNum, allURLs,
                   getLinks, niceDownload)
import Utilities (noInternet, noImages, scrub, invalidTag, addBaseAddress,
                  emptyInput, openURL)
import Find (find)

type URL = String

main :: IO ()
main = do
    args <- cmdArgs r34
    if null $ search args
    then do
        eitherUrl <- askURL (tag args)
        case eitherUrl of
            Left msg -> putStrLn msg
            Right url -> do
                dir <- getDir (directory args)
                firstpage <- try $ openURL url :: IO (Either
                                                      SomeException
                                                      String)
                case firstpage of
                    Left _ -> putStrLn noInternet
                    Right val -> if noImagesExist val
                                    then putStrLn $ noImages url
                                    else do
                        let lastpage = desiredSection start end getPageNum val
                            urls = allURLs url lastpage
                            start = "<section id='paginator'>"
                            end = "</section"
                        links <- takeNLinks args <$> getLinks urls putStr
                        threads <- newMVar []
                        {- don't actually need to do anything with the child
                        threads, as the only way a user can cancel the download
                        is with ctrl+c or similar method which kills the entire
                        program. The haskell runtime kills all threads upon the
                        mainthread dying. -}
                        niceDownload dir links putStr threads
    else do
        eitherResult <- find (search args)
        either putStrLn (mapM_ putStrLn) eitherResult

askURL :: String -> IO (Either String URL)
askURL tag'
    | null tag' = askURL'
    | otherwise = if null $ scrub tag'
                    then return $ Left invalidTag
                    else return . Right $ addBaseAddress (scrub tag')

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

takeNLinks :: R34 -> [URL] -> [URL]
takeNLinks r links
    | first r <= 0 = links
    | otherwise = take (first r) links
