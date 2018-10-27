module Main where

-- Imports
import Control.Concurrent (forkFinally)
import Network.Socket.ByteString (recv)
import Control.Monad (forever, unless)
import qualified Data.ByteString as BS
import Network.Socket hiding (recv)
import Control.Exception (bracket)
import HTTP

-- This is the main function for the server
main :: IO ()
main = do
  -- Ask for an address with the port 3000
  addr <- resolve 3000
  -- This function first acquires a socket resource with `(open addr)` and then
  -- calls `loop` on it. If at any point, an exception is raised, or the loop is
  -- exited, this function ensures that the socket will be closed
  bracket (open addr) close loop
  where
    -- Take a port and return a bindable address
    resolve :: Integer -> IO AddrInfo
    resolve port = do
      let hints = defaultHints {
            -- Allows binding to the socket and the accepting of connections
            -- Makes this the server program
            addrFlags = [AI_PASSIVE],
            -- Options are `Stream` which offers a continous, bidirectional
            -- channel of communication, and `Datagram` which is a one-way,
            -- one-time sending of a packet. TCP requires `Stream`, UDP would
            -- require the use of `Datagram`
            addrSocketType = Stream}
      -- The getAddrInfo function takes some settings in the `hints` record type,
      -- a hostname (if a client), and a service-name (if a server)
      -- See `man getaddrinfo` for more information
      addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
      return addr
    -- Take an address, bind to it, and return socket that can be used to accept
    -- connections from clients
    open :: AddrInfo -> IO Socket
    open addr = do
      -- Get a socket from the address, setting the appropriate flags for a TCP
      -- connection.
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      -- Allows binding to the same port and address even if there is still a
      -- TCP send buffer from a dead server before. This helps prevent the port
      -- from being indefinitely lost in the case of a crash or ungraceful exit.
      setSocketOption sock ReuseAddr 1
      -- This binds a certain address to this socket, ensuring exclusive access
      -- to that address. This socket becomes the program's channel for
      -- communication with the network stack.
      bind sock (addrAddress addr)
      -- Set the socket to listen for incoming connections. Also sets the max
      -- number of queued connects that the system allows.
      listen sock 5
      return sock
    -- Loop forever, accepting any pending connections and forwarding them to
    -- the talk function in a seperate thread.
    loop :: Socket -> IO ()
    loop sock = forever $ do
      -- Accepts a peer connection from the socket queue, returning a new
      -- socket connected to the peer and some information about the peer.
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      -- In a new thread, call the talk function on the connected socket and,
      -- when the thread terminates, close the connection.
      forkFinally (talk conn) (\_ -> close conn)
    -- Read a kilobyte at a time from the client and echo it to stdout until
    -- there is no more data to be read.
    talk :: Socket -> IO ()
    talk conn = do
      -- Get a kilobyte of data from the connection and store it in `msg`
      msg <- recv conn 1024
      -- Continue calling talk until there is no more data to read
      unless (BS.null msg) $ do
        -- Echo the message!
        BS.putStr msg
        -- Recurse until there is nothing more to be read
        talk conn