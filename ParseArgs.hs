{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ParseArgs where

import System.Console.CmdArgs

r34 :: R34
r34 = R34 {
    tag = def &= typ "String" &= help "Downloads all images with the given tag",
    first = def &= typ "Integer" &= help "Downloads the first n images",
    directory = def &= typ "Directory" &= help "Specify the directory to download images to. Defaults to current directory.",
    search = def &= typ "String" &= help "Searches for the given tag and outputs any tags that match the beginning of it."
} &= help "Download images from rule34.paheal\nWill prompt for a tag if none given on command line."
  &= summary "rule34 paheal downloader version 1.0"

data R34 = R34 {
    tag :: String,
    first :: Int,
    directory :: FilePath,
    search :: String
} deriving (Data,Typeable,Show)
