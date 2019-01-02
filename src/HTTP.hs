-- This is where I will write the HTTP type + read and show implementations
module HTTP where

-- Imports
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Util

-- This is a simple Enum type for encapsulating the various HTTP request methods
-- There are many more than these two, but I'm keeping things simple for now
data Method = GET | POST deriving (Show, Read)

-- Just make it clear that this combination of types will be used to store
-- headers and their content. Also allow the derivation of instances.
newtype Header = Header (String, String) deriving Show

-- This `Read` instance allows the parsing of HTTP headers
instance Read Header where
  readsPrec _ str = [(Header (key, val), "")]
    where key = trim . takeWhile (/= ':') $ str
          val = trim . tail . dropWhile (/= ':') $ str

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
          h = map read . takeWhile (not . all isSpace) . drop 1 . lines $ str
          b = pack . trim . unlines . dropWhile (not . all isSpace) . lines $ str

-- This type encapsulates a HTTP response
data Response = Response { version :: Double,
                           status :: Int,
                           headers :: [Header],
                           body :: ByteString }

-- `Show` implementation for `Response` that converts to a proper HTTP string
instance Show Response where
  show (Response v s h b) = "HTTP/" ++ show v ++ " " ++ show s ++ " "
                            ++ decodeStatus s ++ "\n"
                            ++ concatMap showHeader h ++ "\n" ++ unpack b
    where showHeader (Header (key,val)) = key ++ ": " ++ val ++ "\n"

-- A simple function for setting headers and values
setHeader :: String -> String -> [Header] -> [Header]
setHeader key value = (:) (Header (key, value))

-- Resolves status codes into status messages
decodeStatus :: Int -> String
decodeStatus 200 = "OK"
decodeStatus 404 = "Not Found"
decodeStatus 501 = "Not Implemented"
decodeStatus 505 = "HTTP Version Not Supported"
decodeStatus _   = "Unknown Status Code"

-- Take a mime type and body then return a set of sane, generic headers
defaultHeaders :: String -> ByteString -> [Header]
defaultHeaders m b = setHeader "Server" banner .
                     setHeader "Content-Length" (show $ BS.length b) .
                     setHeader "Content-Type" m $ []

-- Create an HTTP response with the given status code, mime type, and body
respond :: Int -> String -> ByteString -> Response
respond s m b = Response 1.1 s (defaultHeaders m b) b

-- Takes a url-encoded form response and returns the key-value pairs
urlToMap :: String -> [(String, String)]
urlToMap = map (toKV . map urlDecode . split "=") . split "&"
  where toKV (x:y:_) = (x,y)
        toKV _ = undefined
