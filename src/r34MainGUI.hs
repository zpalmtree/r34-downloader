{- I could import all the GTK stuff just from Graphics.UI.Gtk (..), but I find
it a lot easier to learn and remember where things are if I export them from
each submodule -}
import Graphics.UI.Gtk.General.General (initGUI, mainQuit, mainGUI,
                                        postGUIAsync)
import Graphics.UI.Gtk.Builder (builderAddFromFile, builderNew,
                                builderGetObject, Builder)
import Graphics.UI.Gtk.Abstract.Object (objectDestroy)
import Graphics.UI.Gtk.Windows.Window (castToWindow, windowDefaultWidth,
                                       Window)
import Graphics.UI.Gtk.Buttons.Button (castToButton, buttonActivated)
import Graphics.UI.Gtk.Entry.Entry (castToEntry, entryGetText)
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox (castToComboBox,
                                                  comboBoxAppendText,
                                                  comboBoxSetModelText,
                                                  comboBoxSetActive,
                                                  comboBoxGetActiveText)
import Graphics.UI.Gtk.Windows.Dialog (dialogRun, ResponseId(..),
                                       dialogResponse)
import Graphics.UI.Gtk.Abstract.Widget (widgetShowAll, widgetDestroy)
import Graphics.UI.Gtk.Windows.MessageDialog (messageDialogNew,
                                              MessageType(..), ButtonsType(..),
                                              MessageDialog,
                                              messageDialogSetMarkup)
import Graphics.UI.Gtk.Selectors.FileChooser (fileChooserSelectFilename,
                                              castToFileChooser,
                                              fileChooserGetFilename)
import System.Glib.Signals (on)
import System.Glib.Attributes (set, AttrOp((:=)))
import Data.Text (Text, pack, unpack)
import Control.Concurrent (killThread, forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import System.Directory (getCurrentDirectory, getPermissions, writable)
import Find (find)
import Utilities (emptySearch, emptyTag, openURL, noInternet, noImagesGUI,
                  addBaseAddress, permissionError)
import MainDriver (noImagesExist, desiredSection, getPageNum, allURLs,
                   getLinks, niceDownload)

main :: IO ()
main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "r34GUI.glade"

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    searchButton <- builderGetObject builder castToButton "searchButton"
    downloadButton <- builderGetObject builder castToButton "downloadButton"
    filePicker <- builderGetObject builder castToFileChooser "folderPicker"

    --need enough room for the title
    set mainWindow [ windowDefaultWidth := 220 ]

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

search :: Builder -> String -> MessageDialog -> IO (Maybe String)
search builder entry _ = do
    eitherResults <- find entry
    case eitherResults of
        Left msg -> return $ Just msg
        Right results -> do
            postGUIAsync $ updateResultsBox builder (map pack results)
            return Nothing

download :: Builder -> String -> MessageDialog -> IO (Maybe String)
download builder tag dialog = do
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
                niceDownload dir links (guiLogger dialog)
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
                    (MessageDialog -> IO (Maybe String)) -> IO ()
cancelableAction mainWindow initMsg completionMsg func = do
    dialog <- newDialog mainWindow ButtonsCancel initMsg
    childThread <- forkIO $ childTasks dialog
    response <- dialogRun dialog
    widgetDestroy dialog
    case response of
        ResponseOk -> do
            dialogDone <- newDialog mainWindow ButtonsClose completionMsg
            void $ dialogRun dialogDone
            widgetDestroy dialogDone

        ResponseNo -> return ()

        --cancel or window closed
        _ -> killThread childThread

    where childTasks dialog = do
                maybeErr <- func dialog
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

updateResultsBox :: Builder -> [Text] -> IO ()
updateResultsBox builder results = do
    resultsComboBox <- builderGetObject builder
                        castToComboBox "resultsComboBox"
    comboBoxSetModelText resultsComboBox
    mapM_ (comboBoxAppendText resultsComboBox) results
    comboBoxSetActive resultsComboBox 0
