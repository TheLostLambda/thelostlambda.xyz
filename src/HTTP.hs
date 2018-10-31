-- This is where I will write the HTTP type + read and show implementations
module HTTP where

-- Imports
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- This is a simple Enum type for encapsulating the various HTTP request methods
-- There are many more than these two, but I'm keeping things simple for now
data Method = GET | POST deriving (Show, Read)

-- Just make it clear that this combination of types will be used to store
-- headers and their content. This is likely another temporary solution.
type Header = (String, String)

-- This type encapsulates a HTTP request
-- NOTE: `String` is almost certainly the wrong type for path, but it should
-- suffice in the meantime (`FilePath` is synonymous with `String`)
data Request = Request { method :: Method,
                         path :: FilePath,
                         version :: Double, -- Convert this to a generic Num?
                         headers :: [Header],
                         body :: ByteString } deriving Show

-- `Read` implementation for `Request` that parses the client request
instance Read Request where
  readsPrec _ str = [(Request m p v h b, "")]
    where m = read . head . words . head . lines $ str
          p = head . tail . words . head . lines $ str
          v = read . drop 5 . last . words . head . lines $ str
          h = [] -- These last two aren't implemented yet
          b = "" -- For simple get requests, they aren't vital

-- This type encapsulates a HTTP response
data Response = Response { version :: Double,
                           status :: Int,
                           headers :: [Header],
                           body :: ByteString }

-- `Show` implementation for `Response` that converts to a proper HTTP string
instance Show Response where
  show (Response v s h b) = "HTTP/" ++ (show v) ++ " " ++ (show s) ++ " "
                            ++ (decodeStatus s) ++ "\n"
                            ++ (concatMap showHeaders h) ++ "\n" ++ (unpack b)
    where showHeaders (k,v) = k ++ ": " ++ v ++ "\n"

-- A simple function for setting headers and values
setHeader :: String -> String -> [Header] -> [Header]
setHeader key value = (:) (key, value)

-- Resolves status codes into status messages
decodeStatus :: Int -> String
decodeStatus 200 = "OK"
decodeStatus 404 = "Not Found"
decodeStatus 505 = "HTTP Version Not Supported"

-- Create an HTTP response that returns OK
ok :: String -> ByteString -> Response
ok mime body = Response 1.1 200 headers body
  -- Here are some default headers that should make sense in most all cases
  where headers = setHeader "Server" "TLL" .
                  setHeader "Content-Length" (show $ BS.length body) .
                  setHeader "Content-Type" mime $ []
                  
