-- This module is dedicated to the sane handling of files
module File where

-- Imports
import Data.Char (toLower)

-- This function simply returns the file part of a filepath
getFile :: FilePath -> FilePath
getFile = reverse . takeWhile (/= '/') . reverse

-- This is a simple function that fetches the type of a file
getFileType :: FilePath -> String
getFileType = map toLower . drop 1 . dropWhile (/= '.') . getFile

-- This function properly concatenates two paths
-- This currently doesn't support `.` or `..` paths in the second position
-- Eventually write an instance that allows `++` to be used for this!
appendPath :: FilePath -> FilePath -> FilePath
appendPath p q
  | isPath p && isRelative q = p ++ q
  | isPath p                 = p ++ tail q
  | otherwise                = p

-- Does this `FilePath` end with a `/`?
isPath :: FilePath -> Bool
isPath = (==) "" . getFile

-- Does this path start with `/` or not?
isRelative :: FilePath -> Bool
isRelative = (/=) '/' . head

-- This function takes a file type and returns the proper mime type and encoding
toMime :: String -> (String, String)
toMime "css"  = ("text/css", "")
toMime "html" = ("text/html", "")
toMime "png"  = ("image/png", "")
toMime "jpeg" = ("image/jpeg", "")
toMime "jpg"  = ("image/jpeg", "")
toMime "txt"  = ("text/plain", "")
toMime "ico"  = ("image/x-icon", "")
toMime "svg"  = ("image/svg+xml", "")
toMime "js"   = ("text/javascript", "")
toMime "pdf"  = ("application/pdf", "")
toMime "svgz" = ("image/svg+xml", "gzip")
toMime _      = ("application/octet-stream", "")
