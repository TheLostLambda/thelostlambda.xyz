-- This module contains the code for the translation of requests to responses
module Router where

-- Imports
import Data.ByteString.Char8 (pack, unpack) -- Gross?
import System.Directory (doesFileExist) -- Try to get rid of this at some point
import qualified Data.ByteString as BS
import HTTP
import File

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
  -- Append the requested URL to the `root` path and, if the URL doesn't contain
  -- a file, default to `index.html`
  let file = if isPath fp then foldl1 appendPath [root, fp, "index.html"] else appendPath root fp
  -- Check if the requested resource exists on the server
  exists <- doesFileExist file
  if exists then
    -- If the file exists, return 200 and the contents of the file
    respond 200 (toMime $ getFileType file) <$> BS.readFile file
  else
    -- Otherwise, return 404 and an error page
    respond 404 "text/html" <$> BS.readFile (appendPath stat "404.html")
-- POST request handlers
route (Request POST "/post/" _ _ b) = return $
  respond 200 (toMime "txt") (pack $ show b ++ "\n\n" ++ show (urlToMap $ unpack b))
-- If all previous attempts at routing have failed, return 501 (Not Implemented)
route _ = return $ respond 501 (toMime "") ""
