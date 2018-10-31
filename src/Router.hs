-- This module contains the code for the translation of requests to responses
module Router where

-- Imports
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import HTTP
import File

-- This stores the path that will eventually resolve as `/` in an HTTP request
root :: FilePath
root = "static/"

-- This function processes requests, fetches the requested data, and formulates
-- an appropriate response. This function deals with the `IO` monad
-- NOTE: Add a way to return 404!
route :: Request -> IO Response
route (Request GET fp _ _ _) = do
  let file = if isPath fp then foldl1 appendPath [root, fp, "index.html"] else appendPath root fp
  exists <- doesFileExist file
  if exists then
    respond 200 (toMime $ getFileType file) <$> BS.readFile file
  else
    respond 404 "text/html" <$> BS.readFile (appendPath root "404/index.html")
route _ = return $ respond 501 (toMime "") ""
