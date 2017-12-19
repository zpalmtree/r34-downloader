module Messages
(
    permissionError,
    noInternet,
    noTags,
    downloading,
    linksAdded,
    noImages,
    maxErrors,
    downloadFail
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

downloading :: Int -> Int -> URL -> String
downloading = printf "Downloading %d out of %d: %s"

linksAdded :: Int -> String
linksAdded = printf "%d links added to download..."

maxErrors :: String
maxErrors = "Error: Max errors in a row exceeded, terminating download"

downloadFail :: Show a => a -> String
downloadFail e = "Error: Download Exception - " ++ show e
