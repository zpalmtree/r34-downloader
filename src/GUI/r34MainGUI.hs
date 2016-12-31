{-
I could import all the GTK stuff just from Graphics.UI.Gtk (..), but I find
it a lot easier to learn and remember where things are if I export them from
each submodule
-}
import Graphics.UI.Gtk.General.General (initGUI, mainQuit, mainGUI)
import Graphics.UI.Gtk.Builder (builderAddFromFile, builderNew,
                                builderGetObject, Builder)
import Graphics.UI.Gtk.Abstract.Object (objectDestroy)
import Graphics.UI.Gtk.Windows.Window (castToWindow, windowDefaultWidth)
import Graphics.UI.Gtk.Buttons.Button (castToButton, buttonActivated)
import Graphics.UI.Gtk.Entry.Entry (castToEntry, entryGetText)
import Graphics.UI.Gtk.Abstract.Widget (widgetShowAll, widgetDestroy)
import Graphics.UI.Gtk.Windows.MessageDialog (messageDialogNew,
                                              MessageType(..), ButtonsType(..))
import Graphics.UI.Gtk.Windows.Dialog (dialogRun)

import System.Glib.Signals (on)
import System.Glib.Attributes (set, AttrOp((:=)))

import Control.Monad (void)

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
    on searchButton buttonActivated $ searchGUI builder

    widgetShowAll mainWindow
    mainGUI

searchGUI :: Builder -> IO ()
searchGUI builder = do
    searchInput <- builderGetObject builder castToEntry "searchInput"
    entry <- entryGetText searchInput :: IO String
    if null entry
    then do
        dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose emptyMsg
        void $ dialogRun dialog
        widgetDestroy dialog
    else do
        --results <- search entry
        --update resultsBox
        return ()
    where emptyMsg = "No tag entered. Please enter one before searching."
