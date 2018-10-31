-- This module contains the code for the translation of requests to responses
module Router where

-- Imports
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
route (Request GET fp _ _ _) = ok (toMime $ getFileType file) <$> body
  where body = BS.readFile $ appendPath root file
        file = if isPath fp then appendPath fp "index.html" else fp
