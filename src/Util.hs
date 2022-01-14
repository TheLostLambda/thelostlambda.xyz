-- This module holds useful odds and ends for use throughout the program
module Util where

-- Imports
import Data.List (findIndex, isPrefixOf, tails)
import Data.Char (isSpace, isHexDigit, chr)
import Data.Maybe (fromJust, isNothing)
import Numeric (readHex)

-- This contains the identification and version information for this server
banner :: String
banner = "TLL v0.1.2.0"

-- The pattern used to offset values for interpolation in HTML templates
tmplStr :: String
tmplStr = "##"

-- This function trims whitespace from the beginning and end of strings
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Splits a string at a given substring and returns a list of parts
split :: String -> String -> [String]
split sub str
  | isNothing subIndex = [str]
  | otherwise = chop (length sub) . fromJust $ subIndex
  where chop len idx = take idx str : split sub (drop (idx + len) str)
        subIndex = findIndex (isPrefixOf sub) $ tails str

-- Produces an interpolated string from a map of values
-- Find a way to generalize the map values without extra quotes from show
interpolate :: String -> [(String, String)] -> String -> String
interpolate pat kvs = concatMap (\x -> lookup x kvs `orElse` x) . split pat

-- Characters that require url-encoding
urlChars :: [Char]
urlChars = ":/?#[]@!$&'()*+,;=% "

-- Takes a percent-encoded string and decodes it
urlDecode :: String -> String
urlDecode = convert "" . map (\c -> if c == '+' then ' ' else c)
  where convert acc str@(x:xs) = if x == '%'
          then convert (acc ++ [urlToASCII $ take 3 str]) (drop 3 str)
          else convert (acc ++ [x]) xs
        convert acc "" = acc

-- Converts a percent encoded character back to ASCII
urlToASCII :: String -> Char
urlToASCII = chr . fst . head . readHex . filter isHexDigit

-- Takes a Maybe and a default value to use if the Maybe is Nothing
orElse :: Maybe a -> a -> a
(Just v) `orElse` _ = v
_ `orElse` v = v
