module Messages
(
    emptyTag,
    noImagesGUI,
    permissionError,
    noInternet,
    noTags,
    invalidTag,
    emptySearch,
    emptyInput,
    tooManyResults,
    downloadException,
    downloading,
    linksAdded,
    noImages
)
where

import Utilities (URL, allowedChars, maxComboBoxSize)
import Text.Printf (printf)

emptyTag :: String
emptyTag = "No tag selected. Please select one before downloading."

noImagesGUI :: String
noImagesGUI = "No images were found with that tag. This is an error which \
              \occurs when a tag exists, but its images have been removed \
              \from the site."

permissionError :: String
permissionError = "You don't have permission to save files to the selected \
                  \folder. Try running the program again with admin \
                  \privileges, if this does not rectify the problem, \
                  \choose another folder."

noInternet :: String 
noInternet = "Sorry, we couldn't connect to the website. Check that it's not \
             \down and you have an internet connection."

noTags :: String
noTags = "No tag found with that search term, please try again."

invalidTag :: String
invalidTag = "Invalid tag entered. Allowed characters are " ++ allowedChars

emptySearch :: String
emptySearch = "No tag entered. Please enter one before searching."

emptyInput :: String
emptyInput = "No text entered. Please enter a tag."

tooManyResults :: String
tooManyResults = printf "Too many results found to be displayed. Truncated to \
                        \%d items. Please refine your search to see all the \
                        \results." maxComboBoxSize

downloadException :: URL -> String -> String
downloadException = printf "Error downloading %s - failed with exception %s"

downloading :: Int -> Int -> URL -> String
downloading = printf "Downloading %d out of %d: %s"

linksAdded :: Int -> String
linksAdded = printf "%d links added to download..."

noImages :: URL -> String
noImages = printf "Sorry - no images were found with that tag. (URL: %s) \
                  \Ensure you spelt it correctly."
