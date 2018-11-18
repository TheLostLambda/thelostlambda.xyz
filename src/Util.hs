-- This module holds useful odds and ends for use throughout the program
module Util where

-- Imports
import Data.Char (isSpace)

-- This function trims whitespace from the beginning and end of strings
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
