module Main
(
    main
)
where

import ParseArgs 
import Download
import Utilities 
import Find
import Messages
import Links

import Control.Exception
import System.Console.CmdArgs (cmdArgs)
import Control.Concurrent
import System.IO
import System.Directory
import System.FilePath

main :: IO ()
main = handleArgs =<< cmdArgs r34

handleArgs :: R34 -> IO ()
handleArgs args
    | null $ search args = mapM_ putStrLn =<< download' args
    | otherwise = either putStrLn (mapM_ putStrLn) =<< find (search args)

askURL :: String -> IO (Either String URL)
askURL tag'
    | null tag' = askURL'
    | null $ scrub tag' = return $ Left invalidTag
    | otherwise = return . Right $ addBaseAddress (scrub tag')

askURL' :: IO (Either String URL)
askURL' = do
    putStr "Enter the tag which you wish to download: "
    hFlush stdout
    handleInput <$> getLine

handleInput :: String -> Either String URL
handleInput input
    | null input = Left emptyInput
    | null $ scrub input = Left invalidTag
    | otherwise = Right . addBaseAddress $ scrub input

getDir :: FilePath -> IO FilePath
getDir dir = do
    isDir <- doesDirectoryExist dir
    if isDir
        then return $ a dir
        else a <$> getCurrentDirectory
    where a = addTrailingPathSeparator

download' :: R34 -> IO (Maybe String)
download' args = do
    url <- askURL (tag args)
    dir <- getDir (directory args)
    firstpage <- runEitherIO url openURLWrapped
    let url' = runEither firstpage (checkForImages $ fromRight url)
    case url' of
        Left msg -> return $ Just msg
        Right url'' -> do
            imageLinks <- getImageLinks url'' putStrLn
            if disableAsync args
                then download dir imageLinks putStrLn =<< newEmptyMVar
                else downloadAsync dir imageLinks putStrLn =<< newMVar []
            return Nothing

runEitherIO :: Either a b -> (b -> IO (Either a b)) -> IO (Either a b)
runEitherIO (Right something) f = f something
runEitherIO left _ = return left

runEither :: Either a b -> (b -> Either a b) -> Either a b
runEither (Right something) f = f something
runEither left _ = left

openURLWrapped :: URL -> IO (Either String String)
openURLWrapped url = do
    page <- try $ openURL url :: IO (Either SomeException String)
    case page of
        Left _ -> return $ Left noInternet
        --need to update the implicit left type
        Right str -> return $ Right str

checkForImages :: URL -> String -> Either String String
checkForImages url page
    | noImagesExist page = Left (noImages url)
    | otherwise = Right url

fromRight :: Either a b -> b
fromRight (Right val) = val
