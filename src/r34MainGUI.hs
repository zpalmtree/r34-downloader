{-
I could import all the GTK stuff just from Graphics.UI.Gtk (..), but I find
it a lot easier to learn and remember where things are if I export them from
each submodule
-}
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
                                                  comboBoxSetActive)
import Graphics.UI.Gtk.Windows.Dialog (dialogRun, ResponseId(..),
                                       dialogResponse)
import Graphics.UI.Gtk.Abstract.Widget (widgetShowAll, widgetDestroy)
import Graphics.UI.Gtk.Windows.MessageDialog (messageDialogNew,
                                              MessageType(..), ButtonsType(..),
                                              MessageDialog)


import System.Glib.Signals (on)
import System.Glib.Attributes (set, AttrOp((:=)))

import Data.Text (Text, pack)

import Control.Concurrent (forkIO, killThread)

import Control.Monad (void)

import Find (find)
import Utilities (emptySearch)

main :: IO ()
main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "r34GUI.glade"

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    --need enough room for the title
    set mainWindow [ windowDefaultWidth := 220 ]

    searchButton <- builderGetObject builder castToButton "searchButton"

    on mainWindow objectDestroy mainQuit
    on searchButton buttonActivated $ searchGUI builder mainWindow

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

search :: Builder -> String -> IO (Maybe String)
search builder entry = do
    eitherResults <- find entry
    case eitherResults of
        Left msg -> return $ Just msg
        Right results -> do
            postGUIAsync $ updateResultsBox builder (map pack results)
            return Nothing


alert :: String -> Window -> IO ()
alert msg mainWindow = do
    dialog <- messageDialogNew (Just mainWindow) []
                MessageInfo ButtonsClose msg
    void $ dialogRun dialog
    widgetDestroy dialog

cancelableAction :: Window -> String -> String -> IO (Maybe String) -> IO ()
cancelableAction mainWindow initMsg completionMsg func = do
    dialog <- newDialog mainWindow ButtonsCancel initMsg
    childThread <- forkIO (childTasks dialog)
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
                maybeErr <- func
                case maybeErr of
                    Nothing -> postGUIAsync $ dialogResponse dialog ResponseOk
                    Just errMsg -> do
                        postGUIAsync (alert errMsg mainWindow)
                        postGUIAsync $ dialogResponse dialog ResponseNo

newDialog :: Window -> ButtonsType -> String -> IO MessageDialog
newDialog mainWindow = messageDialogNew (Just mainWindow) [] MessageInfo

updateResultsBox :: Builder -> [Text] -> IO ()
updateResultsBox builder results = do
    resultsComboBox <- builderGetObject builder
                        castToComboBox "resultsComboBox"
    comboBoxSetModelText resultsComboBox
    mapM_ (comboBoxAppendText resultsComboBox) results
    comboBoxSetActive resultsComboBox 0
