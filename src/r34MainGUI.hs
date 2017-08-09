{-# LANGUAGE GADTs #-}

module Main
(
    main
)
where

import Graphics.QML

import Data.Text (pack, unpack, Text)
import Control.Concurrent (forkIO)
import Control.Exception (evaluate, try, SomeException)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Concurrent.Thread.Delay (delay)
import System.Directory (getPermissions, writable)
import Control.Monad (when, void)
import Data.List (stripPrefix)

import Find (find)
import Messages (permissionError, noInternet, noImagesGUI)
import Utilities (addBaseAddress, noImagesExist, openURL)
import Links (getImageLinks)

import Paths_rule34_paheal_downloader (getDataFileName)

data StatesNSignals = StatesNSignals {
    searchSignal :: SignalKey (IO ()),
    mbTextSignal :: SignalKey (IO ()),
    mbVisibleSignal :: SignalKey (IO ()),
    mbButtonsSignal :: SignalKey (IO ()),
    searchState :: IORef [Text],
    mbTextState :: IORef Text,
    mbVisibleState :: IORef Bool,
    mbButtonsState :: IORef Text
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

    let s = StatesNSignals searchSig mbTextSig mbVisibleSig mbButtonsSig
                           searchS mbTextS mbVisibleS mbButtonsS
    
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

        defMethod' "download" (downloadMethod s)]

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
    writeMsg s this "Searching..." "NoButton"

    forkIO $ do
        results <- find $ unpack searchTerm
        case results of
            Left err -> writeMsg s this err "Ok"
            Right results' -> do
                -- is this needed?
                evaluate results'

                hideMsg s this

                writeIORef (searchState s) (map pack results')
                fireSignal (searchSignal s) this
    return ()

downloadMethod :: (MarshalMode tt ICanPassTo () ~ Yes, 
                   MarshalMode tt IIsObjType () ~ Yes, 
                   Marshal tt) => StatesNSignals -> tt -> Text -> Text -> IO ()
downloadMethod s this tag' folder' = do
    let tag = unpack tag'
        Just folder = fmap (++ "/") . stripPrefix "file://" $ unpack folder'

    permissions <- getPermissions folder

    if not $ writable permissions
        then writeMsg s this permissionError "Ok"
        --fork thread so GUI stays responsive
        else void . forkIO $ do
            writeMsg s this "Finding links..." "Cancel"
            let url = addBaseAddress tag
            firstpage <- try $ openURL url :: IO (Either SomeException String)
            case firstpage of
                Left _ -> writeMsg s this noInternet "Ok"
                Right val -> if noImagesExist val
                    then writeMsg s this noImagesGUI "Ok"
                    else do
                        imageLinks <- getImageLinks url (guiLogger s this)
                        print imageLinks
                        hideMsg s this

writeMsg :: (MarshalMode tt IIsObjType () ~ Yes, 
             MarshalMode tt ICanPassTo () ~ Yes, 
             Marshal tt) => StatesNSignals -> tt -> String -> String -> IO ()
writeMsg s this msg buttonType = do
    hideMsg s this

    --window doesn't reappear unless we delay a bit
    delay 1000

    writeIORef (mbTextState s) (pack msg)
    writeIORef (mbButtonsState s) (pack buttonType)
    writeIORef (mbVisibleState s) True

    --do we need to fire all signals, or just firing the visible will work?
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
