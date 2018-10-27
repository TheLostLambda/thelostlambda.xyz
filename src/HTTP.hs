-- This is where I will write the HTTP type + read and show implementations
module HTTP where

-- Imports
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (chr)

-- This is a simple Enum type for encapsulating the various HTTP request methods
-- There are many more than these two, but I'm keeping things simple for now
data Method = GET | POST deriving (Show, Read)

-- Just make it clear that this combination of types will be used to store
-- headers and their content. This is likely another temporary solution.
type Header = (String, String)

-- This type encapsulates a HTTP request
-- NOTE: `String` is almost certainly the wrong type for path, but it should
-- suffice in the meantime
data Request = Request { method :: Method,
                         path :: String,
                         version :: Double, -- Convert this to a generic Num?
                         headers :: [Header],
                         body :: ByteString }

-- This type encapsulates a HTTP response
data Response = Response { version :: Double,
                           status :: Int,
                           headers :: [Header],
                           body :: ByteString }

-- `Show` implementation for `Response` that converts to a proper HTTP string
-- Wow, comment all of this. Especially the bs2str bit.
instance Show Response where
  show (Response v s h b) = "HTTP/" ++ (show v) ++ " " ++ (show s) ++ " "
                            ++ (decodeStatus s) ++ "\n"
                            ++ (concatMap showHeaders h) ++ "\n" ++ (bs2str b)
    where showHeaders (k,v) = k ++ ": " ++ v ++ "\n"
          bs2str bs = map (chr . fromEnum) . BS.unpack $ bs

-- Resolves status codes into status messages
decodeStatus :: Int -> String
decodeStatus 200 = "OK"
decodeStatus 404 = "Not Found"
decodeStatus 505 = "HTTP Version Not Supported"
