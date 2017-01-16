module Utilities
(
    noInternet,
    noTags,
    openURL,
    invalidTag,
    filetypes,
    oneSecond,
    noImages,
    addBaseAddress,
    isAllowedChar,
    replaceSpace,
    removeEscapeSequences,
    emptySearch,
    emptyTag,
    noImagesGUI,
    permissionError,
    zipWithM3_,
    maxComboBoxSize,
    tooManyResults,
    emptyInput,
    scrub
)
where

import Network.HTTP (getResponseBody, simpleHTTP, getRequest)
import Text.Printf (printf)
import Data.Foldable (sequenceA_)

type URL = String

emptyTag :: String
emptyTag = "No tag selected. Please select one before downloading."

noImagesGUI :: String
noImagesGUI = "No images were found with that tag. This is an error which occurs when a tag exists, but its images have been removed from the site."

permissionError :: String
permissionError = "You don't have permission to save files to the selected folder. Try running the program again with admin privileges, if this does not rectify the problem, choose another folder."

noInternet :: String 
noInternet = "Sorry, we couldn't connect to the website. Check that it's not \
            \down and you have an internet connection."

noTags :: String
noTags = "No tag found with that search term, please try again."

invalidTag :: String
invalidTag = printf "Invalid tag entered. \
                           \Allowed characters are %s" allowedChars

emptySearch :: String
emptySearch = "No tag entered. Please enter one before searching."

emptyInput :: String
emptyInput = "No text entered. Please enter a tag."

tooManyResults :: String
tooManyResults = printf "Too many results found to be displayed. Truncated to \
                 \%d items. Please refine your search to see all the results."
                 maxComboBoxSize
                    

filetypes :: [String]
filetypes = [".jpg", ".png", ".gif", ".jpeg"]

openURL :: URL -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

--1 second in milliseconds
oneSecond :: (Num a) => a
oneSecond = 1000000

addBaseAddress :: String -> URL
addBaseAddress xs = "http://rule34.paheal.net/post/list/" ++ xs ++ "/1"

scrub :: String -> String
scrub = filter isAllowedChar . map replaceSpace

noImages :: URL -> String
noImages = printf "Sorry - no images were found with that tag. (URL: %s) \
            \Ensure you spelt it correctly."

--comment for hlint, style checker
--makes a lot more sense for this to be an array of characters than a string
{-# ANN allowedChars "HLint: ignore" #-}
allowedChars :: [Char]
allowedChars = ['_', '\'', '-', '.', ':', '@', '+'] ++ ['a'..'z'] ++
               ['A'..'Z'] ++ ['0'..'9']

isAllowedChar :: Char -> Bool
isAllowedChar = flip elem allowedChars

--Replace spaces with underscores so tag searching is more user friendly
replaceSpace :: Char -> Char
replaceSpace ' ' = '_'
replaceSpace c = c

removeEscapeSequences :: String -> String
removeEscapeSequences [] = []
removeEscapeSequences ('%':a:b:rest) =
    case code of
        "20" -> go '_'
        "21" -> go '!'
        "23" -> go '#'
        "24" -> go '$'
        "26" -> go '&'
        "27" -> go '\''
        "28" -> go '('
        "29" -> go ')'
        "2A" -> go '*'
        "2B" -> go '+'
        "2C" -> go ','
        "2F" -> go '/'
        "3A" -> go ':'
        "3B" -> go ';'
        "3D" -> go '='
        "3F" -> go '?'
        "40" -> go '@'
        "5B" -> go '['
        "5D" -> go ']'
        "&"  -> go '_'
        _   -> '%' : a : b : removeEscapeSequences rest
    where code = a : [b]
          go c = c : removeEscapeSequences rest

removeEscapeSequences (c:cs) = c : removeEscapeSequences cs

zipWithM3_ :: (Applicative m) => 
              (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWithM3_ f xs ys zs = sequenceA_ $ zipWith3 f xs ys zs

maxComboBoxSize :: Int
maxComboBoxSize = 200
