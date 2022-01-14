-- This module contains the code for the translation of requests to responses
module Router where

-- Imports
import Data.ByteString.Char8 (pack, unpack) -- Gross?
import System.Directory (doesFileExist) -- Try to get rid of this at some point
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import HTTP
import File
import Util

-- This stores the path that will eventually resolve as `/` in an HTTP request
root :: FilePath
root = "web/page/"

-- This stores the path that the status code pages live in (404, 401, etc.)
stat :: FilePath
stat = "web/error/"

-- This function processes requests, fetches the requested data, and formulates
-- an appropriate response. This function deals in the `IO` monad, so it is able
-- to read and write files on the disk
route :: Request -> IO Response
-- Static files router
route (Request GET fp _ _ _) = do
  -- Generate the path to the theme file
  let theme = appendPath root "app.html"  
  -- Append the requested URL to the root path
  let url = appendPath root fp
  -- Check if the requested resource exists on the server
  isFile <- doesFileExist url
  -- If it doesn't, try adding index.html to the end
  let file = if not isFile then appendPath (url ++ "/") "index.html" else url
  -- Does it exist now?
  exists <- doesFileExist file
  if exists then do
    let fileType = toMime $ getFileType file
    -- If the file exists, return 200 and the requested resource
    case fst fileType of
      -- If it's HTML, wrap it in a theme
      "text/html" -> respond 200 fileType <$> (withTheme theme =<< BS.readFile file)
      -- Otherwise just return the file
      _ -> respond 200 fileType <$> BS.readFile file
  else
    -- Otherwise, return 404 and an error page
    respond 404 (toMime "html") <$> (withTheme theme =<< BS.readFile (appendPath stat "404.html"))
-- POST request handlers
route (Request POST "/post" _ _ b) = do
   -- Generate the path to the theme file
  let theme = appendPath root "app.html"
  file <- unpack <$> BS.readFile (appendPath root "/post/post.html")
  respond 200 (toMime "html") <$> (withTheme theme . pack $ interpolate tmplStr (urlToMap $ unpack b) file)
-- If all previous attempts at routing have failed, return 501 (Not Implemented)
route _ = return $ respond 501 (toMime "") ""

-- This function takes a page and wraps it in a common header and footer
-- I need to make the interpolate function work on ByteStrings
withTheme :: FilePath -> ByteString -> IO ByteString
withTheme tp file = do
  theme <- BS.readFile tp
  return . pack . interpolate tmplStr [("BODY", unpack file)] . unpack $ theme
