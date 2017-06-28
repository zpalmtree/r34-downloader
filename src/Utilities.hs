module Utilities
(
    openURL,
    filetypes,
    oneSecond,
    isAllowedChar,
    allowedChars,
    replaceSpace,
    removeEscapeSequences,
    zipWithM3_,
    maxComboBoxSize,
    scrub,
    noImagesExist,
    URL
)
where

import Network.HTTP
import Data.Foldable
import Text.HTML.TagSoup

type URL = String

filetypes :: [String]
filetypes = [".jpg", ".png", ".gif", ".jpeg", ".webm"]

openURL :: URL -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

oneSecond :: (Num a) => a
oneSecond = 1000000

scrub :: String -> String
scrub = filter isAllowedChar . map replaceSpace

--makes a lot more sense for this to be an array of characters than a string
{-# ANN allowedChars "HLint: ignore" #-}
allowedChars :: [Char]
allowedChars = ['_', '\'', '-', '.', ':', '@', '+'] ++ ['a'..'z'] ++
               ['A'..'Z'] ++ ['0'..'9']

isAllowedChar :: Char -> Bool
isAllowedChar = flip elem allowedChars

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

noImagesExist :: String -> Bool
noImagesExist page = not $ null . findError $ parseTags page
    where findError = dropWhile (~/= "<section id='Errormain'>")
