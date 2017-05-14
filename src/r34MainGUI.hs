import Graphics.UI.Gtk hiding (response)

import Data.Text (Text, pack, unpack)
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Directory
import Paths_rule34_paheal_downloader
import Find
import Utilities 
import MainDriver 
import Strings

main :: IO ()
main = do
    initGUI

    builder <- builderNew
    -- getDataFileName gets the location of the installed .glade file.
    xmlLocation <- getDataFileName "src/r34GUI.glade"
    builderAddFromFile builder xmlLocation

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    searchButton <- builderGetObject builder castToButton "searchButton"
    downloadButton <- builderGetObject builder castToButton "downloadButton"
    filePicker <- builderGetObject builder castToFileChooser "folderPicker"

    cwd <- getCurrentDirectory
    void $ fileChooserSelectFilename filePicker cwd

    on mainWindow objectDestroy mainQuit
    on searchButton buttonActivated $ searchGUI builder mainWindow
    on downloadButton buttonActivated $ downloadGUI builder mainWindow

    widgetShowAll mainWindow
    mainGUI

searchGUI :: Builder -> Window -> IO ()
searchGUI builder mainWindow = do
    searchInput <- builderGetObject builder castToEntry "searchInput"
    entry <- entryGetText searchInput
    entrySetText searchInput (scrub entry)
    if null entry
        then alert emptySearch mainWindow
        else cancelableAction mainWindow "Searching..." "Done!"
                (search builder entry)

downloadGUI :: Builder -> Window -> IO ()
downloadGUI builder mainWindow = do
    resultsComboBox <- builderGetObject builder
                        castToComboBox "resultsComboBox"
    maybeTag <- comboBoxGetActiveText resultsComboBox
    case maybeTag of
        Nothing -> alert emptyTag mainWindow
        Just tag -> cancelableAction mainWindow "Downloading..." "Done!"
                    (download builder (unpack tag))

search :: Builder -> String -> MessageDialog -> MVar [ThreadId] -> MVar ()
          -> IO (Maybe String)
search builder entry _ _ _ = do
    eitherResults <- fmap (map pack) <$> find entry
    case eitherResults of
        Left msg -> return $ Just msg
        Right results -> do
            validLength <- updateResultsBox results builder
            if validLength
                then return Nothing
                else return $ Just tooManyResults

download :: Builder -> String -> MessageDialog -> MVar [ThreadId] -> MVar ()
            -> IO (Maybe String)
download builder tag dialog threads timeToDie = do
    filePicker <- builderGetObject builder castToFileChooser "folderPicker"
    Just dir <- fmap (++ "/") <$> fileChooserGetFilename filePicker
    permissions <- getPermissions dir

    if writable permissions
        then do
        firstpage <- try $ openURL url :: IO (Either SomeException String)
        case firstpage of
            Left _ -> return $ Just noInternet
            Right val -> if noImagesExist val
                            --sometimes tag can exist with no images
                            then return $ Just noImagesGUI
                            else do

                let lastpage = desiredSection start end getPageNum val
                    urls = allURLs url lastpage
                    start = "<section id='paginator'>"
                    end = "</section"

                links <- getLinks urls (guiLogger dialog)
                checkButton <- builderGetObject builder castToCheckButton 
                               "asyncButton"
                asyncDisabled <- toggleButtonGetActive checkButton
                if asyncDisabled
                    then niceDownload dir links (guiLogger dialog) timeToDie
                    else niceDownloadAsync dir links (guiLogger dialog) threads
                return Nothing

        else return $ Just permissionError
    where url = addBaseAddress tag

guiLogger :: MessageDialog -> String -> IO ()
guiLogger dialog msg = postGUIAsync $ messageDialogSetMarkup dialog msg

{- dialog is ran after childthread is created. This may be dangerous, if
the function returns very quickly it could maybe? give a signal to the
dialog to close before the dialog has been run. I'm not sure what will
happen if this occurs, maybe a crash, or maybe the signal won't be
recognised yet so the dialog will persist despite the operation having
completed. -}
cancelableAction :: Window -> String -> String ->
                    (MessageDialog -> MVar [ThreadId] -> MVar () ->
                    IO (Maybe String)) -> IO ()
cancelableAction mainWindow initMsg completionMsg func = do
    threads <- newMVar []
    timeToDie <- newEmptyMVar
    dialog <- newDialog mainWindow ButtonsCancel initMsg
    childThread <- forkIO $ childTasks dialog threads timeToDie
    response <- dialogRun dialog
    widgetDestroy dialog
    case response of
        ResponseOk -> do
            dialogDone <- newDialog mainWindow ButtonsClose completionMsg
            void $ dialogRun dialogDone
            widgetDestroy dialogDone

        ResponseNo -> return ()

        --cancel or window closed
        _ -> do
            runningThreads <- takeMVar threads
            mapM_ killThread runningThreads
            putMVar timeToDie ()
            killThread childThread

    where childTasks dialog threads timeToDie = do
                maybeErr <- func dialog threads timeToDie
                case maybeErr of
                    Nothing -> postGUIAsync $ dialogResponse dialog ResponseOk
                    Just errMsg -> do
                        postGUIAsync $ alert errMsg mainWindow
                        postGUIAsync $ dialogResponse dialog ResponseNo

alert :: String -> Window -> IO ()
alert msg mainWindow = do
    dialog <- messageDialogNew (Just mainWindow) []
                MessageInfo ButtonsClose msg
    void $ dialogRun dialog
    widgetDestroy dialog

newDialog :: Window -> ButtonsType -> String -> IO MessageDialog
newDialog mainWindow = messageDialogNew (Just mainWindow) [] MessageInfo

updateResultsBox :: [Text] -> Builder -> IO Bool
updateResultsBox results builder
    | length results > maxComboBoxSize = u >> return False
    | otherwise = u >> return True
    where cappedResults = take maxComboBoxSize results
          u = postGUIAsync updateResultsBox'
          updateResultsBox' = do
            resultsComboBox <- builderGetObject builder
                                castToComboBox "resultsComboBox"
            comboBoxSetModelText resultsComboBox
            mapM_ (comboBoxAppendText resultsComboBox) cappedResults
            comboBoxSetActive resultsComboBox 0
