module Main
(
    main
)
where

import Graphics.QML
import Data.Text (pack, unpack)
import Find (find)
import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Data.IORef (newIORef, readIORef, writeIORef)
import Paths_rule34_paheal_downloader (getDataFileName)
import Control.Concurrent.Thread.Delay (delay)

main :: IO ()
main = do
    gui <- getDataFileName "src/main.qml"

    searchSignal <- newSignalKey
    mbTextSignal <- newSignalKey :: IO (SignalKey (IO ()))
    mbVisibleSignal <- newSignalKey
    mbButtonsSignal <- newSignalKey :: IO (SignalKey (IO ()))

    searchState <- newIORef $ map pack [""]
    mbTextState <- newIORef $ pack ""
    mbVisibleState <- newIORef False
    mbButtonsState <- newIORef $ pack "NoButton"
    
    rootClass <- newClass [
        defPropertySigRO' "searchResults" searchSignal $ defRead searchState,

        defPropertySigRO' "msgText" mbTextSignal $ defRead mbTextState,

        defPropertySigRO' "msgVisible" mbVisibleSignal $ defRead mbVisibleState,

        defPropertySigRO' "msgButtons" mbButtonsSignal $ defRead mbButtonsState,

        defMethod' "search" (\this searchTerm -> do
            writeIORef mbTextState (pack "Searching...")
            writeIORef mbButtonsState (pack "NoButton")

            let buttonBatch = do
                    writeIORef mbVisibleState True
                    fireSignal mbTextSignal this
                    fireSignal mbButtonsSignal this
                    fireSignal mbVisibleSignal this

            buttonBatch

            forkIO $ do
                results <- find $ unpack searchTerm
                case results of
                    Left err -> do
                        writeIORef mbVisibleState False
                        fireSignal mbVisibleSignal this

                        --window doesn't reappear unless we delay a bit
                        delay 100000

                        writeIORef mbTextState (pack err)                 
                        writeIORef mbButtonsState (pack "Ok")
                        buttonBatch
                        
                    Right results' -> do
                        evaluate results'

                        writeIORef mbVisibleState False
                        fireSignal mbVisibleSignal this

                        writeIORef searchState (map pack results')
                        fireSignal searchSignal this
            return ())
        ]

    ctx <- newObject rootClass ()

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument gui,
        contextObject = Just $ anyObjRef ctx
    }

    where defRead s _ = readIORef s
