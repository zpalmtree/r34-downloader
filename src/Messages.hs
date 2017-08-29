module Messages
(
    permissionError,
    noInternet,
    noTags,
    downloadException,
    downloading,
    linksAdded,
    noImages
)
where

import Text.Printf (printf)

import Utilities (URL)

noImages :: String
noImages = "No images were found with that tag. This is an error which \
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

downloadException :: URL -> String -> String
downloadException = printf "Error downloading %s - failed with exception %s"

downloading :: Int -> Int -> URL -> String
downloading = printf "Downloading %d out of %d: %s"

linksAdded :: Int -> String
linksAdded = printf "%d links added to download..."
