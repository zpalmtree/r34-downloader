{-# LANGUAGE GADTs #-}

module Main
(
    main
)
where

import Graphics.QML (SignalKey, MarshalMode, IIsObjType, Yes, ICanPassTo, 
                     Marshal, newSignalKey, newClass, defPropertySigRO', 
                     defMethod', newObject, runEngineLoop, defaultEngineConfig,
                     fileDocument, anyObjRef, fireSignal, initialDocument, 
                     contextObject)

import Data.Text (Text, pack, unpack)
import Control.Concurrent (ThreadId, MVar, forkIO, killThread, newEmptyMVar,
                           tryTakeMVar, putMVar, isEmptyMVar, swapMVar)
import Control.Exception (IOException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent.Thread.Delay (delay)
import System.Directory (getPermissions, writable)
import Control.Monad (when, void)
import Data.List (stripPrefix)

import Find (find)
import Messages (permissionError, noInternet, noImages)
import Utilities (addBaseAddress, noImagesExist, openURL, replaceSpace)
import Links (getImageLinks)
import Download (download)

import Paths_rule34_paheal_downloader (getDataFileName)

data StatesNSignals = StatesNSignals {
    searchSignal :: SignalKey (IO ()),
    mbTextSignal :: SignalKey (IO ()),
    mbVisibleSignal :: SignalKey (IO ()),
    mbButtonsSignal :: SignalKey (IO ()),
    searchState :: IORef [Text],
    mbTextState :: IORef Text,
    mbVisibleState :: IORef Bool,
    mbButtonsState :: IORef Text,
    threadMVar :: MVar ThreadId
}

main :: IO ()
main = do
    gui <- getDataFileName "src/main.qml"

    searchSig <- newSignalKey
    mbTextSig <- newSignalKey :: IO (SignalKey (IO ()))
    mbVisibleSig <- newSignalKey
    mbButtonsSig <- newSignalKey :: IO (SignalKey (IO ()))

    searchS <- newIORef $ map pack [""]
    mbTextS <- newIORef $ pack ""
    mbVisibleS <- newIORef False
    mbButtonsS <- newIORef $ pack "NoButton"

    thread <- newEmptyMVar

    let s = StatesNSignals searchSig mbTextSig mbVisibleSig mbButtonsSig
                           searchS mbTextS mbVisibleS mbButtonsS thread
    
    rootClass <- newClass [
        defPropertySigRO' "searchResults" (searchSignal s) 
                        $ defRead (searchState s),

        defPropertySigRO' "msgText" (mbTextSignal s) 
                        $ defRead (mbTextState s),

        defPropertySigRO' "msgVisible" (mbVisibleSignal s) 
                        $ defRead (mbVisibleState s),

        defPropertySigRO' "msgButtons" (mbButtonsSignal s) 
                        $ defRead (mbButtonsState s),

        defMethod' "search" (searchMethod s),

        defMethod' "download" (downloadMethod s),
        
        defMethod' "cancel" (cancelMethod s)]

    ctx <- newObject rootClass ()

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    where defRead s _ = readIORef s

guiLogger :: (MarshalMode tt IIsObjType () ~ Yes, 
              MarshalMode tt ICanPassTo () ~ Yes, 
              Marshal tt) => StatesNSignals -> tt -> String -> IO ()
guiLogger s this msg = writeMsg s this msg "Cancel"

searchMethod :: (MarshalMode tt IIsObjType () ~ Yes, 
                 MarshalMode tt ICanPassTo () ~ Yes, 
                 Marshal tt) => StatesNSignals -> tt -> Text -> IO ()
searchMethod s this searchTerm = do
    writeMsg s this "Searching..." "Cancel"

    threadId <- forkIO $ do
        results <- find . map replaceSpace $ unpack searchTerm
        case results of
            Left err -> writeMsg s this err "Ok"
            Right results' -> do
                hideMsg s this

                writeIORef (searchState s) (map pack results')
                fireSignal (searchSignal s) this

    empty <- isEmptyMVar (threadMVar s)
    if empty
        then putMVar (threadMVar s) threadId
        else void $ swapMVar (threadMVar s) threadId

downloadMethod :: (MarshalMode tt ICanPassTo () ~ Yes, 
                   MarshalMode tt IIsObjType () ~ Yes, 
                   Marshal tt) => StatesNSignals -> tt -> 
                   Text -> Text -> IO ()
downloadMethod s this tag' folder' = do
    let tag = unpack tag'
        Just folder = fmap (++ "/") . stripPrefix "file://" $ unpack folder'

    permissions <- getPermissions folder

    if not $ writable permissions
        then writeMsg s this permissionError "Ok"
        else do
            threadId <- forkIO $ do
                writeMsg s this "Finding links..." "Cancel"
                let url = addBaseAddress tag
                firstpage <- try $ openURL url :: IO (Either IOException
                                                             String)
                case firstpage of
                    Left _ -> writeMsg s this noInternet "Ok"
                    Right val -> if noImagesExist val
                        then writeMsg s this noImages "Ok"
                        else do
                            imageLinks <- getImageLinks url (guiLogger s this)
                            download folder imageLinks (guiLogger s this)

            empty <- isEmptyMVar (threadMVar s)
            if empty
                then putMVar (threadMVar s) threadId
                else void $ swapMVar (threadMVar s) threadId

writeMsg :: (MarshalMode tt IIsObjType () ~ Yes, 
             MarshalMode tt ICanPassTo () ~ Yes, 
             Marshal tt) => StatesNSignals -> tt -> String -> String -> IO ()
writeMsg s this msg buttonType = do
    hideMsg s this

    writeIORef (mbTextState s) (pack msg)
    writeIORef (mbButtonsState s) (pack buttonType)
    writeIORef (mbVisibleState s) True

    fireSignal (mbTextSignal s) this
    fireSignal (mbButtonsSignal s) this
    fireSignal (mbVisibleSignal s) this

hideMsg :: (MarshalMode tt IIsObjType () ~ Yes, 
            MarshalMode tt ICanPassTo () ~ Yes, 
            Marshal tt) => StatesNSignals -> tt -> IO ()
hideMsg s this = do
    windowEnabled <- readIORef (mbVisibleState s)
    when windowEnabled $ do
        writeIORef (mbVisibleState s) False
        fireSignal (mbVisibleSignal s) this
        --window doesn't reappear unless we delay a bit
        delay 1000

cancelMethod :: (MarshalMode tt ICanPassTo () ~ Yes, 
                 MarshalMode tt IIsObjType () ~ Yes, 
                 Marshal tt) => StatesNSignals -> tt -> IO ()
cancelMethod s this = do
    maybeMVar <- tryTakeMVar (threadMVar s)
    case maybeMVar of
        Nothing -> return ()
        Just thread -> do
            killThread thread
            hideMsg s this
