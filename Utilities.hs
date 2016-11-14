module Utilities
( noInternet
, noTags
, openURL
, invalidSearchTerm
, filetypes
, oneSecond
, noImages
, addBaseAddress
, isAllowedChar
, replaceSpace
, removeEscapeSequences
) where

import Network.HTTP (getResponseBody, simpleHTTP, getRequest)
import Text.Printf (printf)

type URL = String

noInternet :: String 
noInternet = "Sorry, we couldn't connect to the website. Check that it's not \
            \down and you have an internet connection."

noTags :: String
noTags = "No tag found with that search term, please try again."

invalidSearchTerm :: String
invalidSearchTerm = "No search term entered, or invalid search term entered\
                    \ , exiting."

filetypes :: [String]
filetypes = [".jpg", ".png", ".gif", ".jpeg"]

openURL :: URL -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

--1 second in milliseconds
oneSecond :: (Num a) => a
oneSecond = 1000000

addBaseAddress :: String -> URL
addBaseAddress xs = "http://rule34.paheal.net/post/list/" ++ xs ++ "/1"

noImages :: URL -> String
noImages = printf "Sorry - no images were found with that tag. (URL: %s) \
            \Ensure you spelt it correctly."

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
        _   -> '%' : a : b : removeEscapeSequences rest
    where code = a : [b]
          go c = c : removeEscapeSequences rest

removeEscapeSequences (c:cs) = c : removeEscapeSequences cs
