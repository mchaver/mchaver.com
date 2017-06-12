---
title: Haskell Network Programming TCP Client and Server
---

Using the [network](https://hackage.haskell.org/package/network) package we can 
build a low level TCP server and client. We will make a simple echo server and 
client. The client sends a message, the server receives the message and sends 
the message back to the client, then the client receives the message it sent.

\begin{code}
import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

runTCPEchoServer :: IO ()
runTCPEchoServer = do 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
\end{code}

It is important to remember to use `Stream` to receive data and respond via TCP. 
Then we can bind the socket to the address, wait to receive data and then 
respond to the client.

\begin{code}
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  (conn, _) <- accept sock
  print "TCP server is waiting for a message..."
  msg <- recv conn 1024
  unless (BS.null msg) $ do 
    print ("TCP server received: " ++ C.unpack msg)
    print "TCP server is now sending a message to the client"
    sendAll conn msg
  print "TCP server socket is closing now."
  close conn
  close sock
\end{code}

The client code is similar. We need to sned data via `Datagram` or the server 
will ignore it. Then we run `sendAll` with a `ByteString` and the server will 
receive the message.

\begin{code}
sendMessage :: String -> IO ()
sendMessage s = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack s
  msg <- recv sock 1024
  close sock
  -- delay thread to avoid client and server from printing at the same time
  threadDelay 1000000
  print ("TCP client received: " ++ C.unpack msg)
\end{code}

We run the server in a separate thread because `recv` is blocking. We add a few
`threadDelay`s to make sure that the server has started up and that the `print`s
from different threads do not occur at the same time. Otherwise, the messages 
might print at the same time and be illegible. 

\begin{code}
main :: IO ()
main = do
  _ <- forkIO $ runTCPEchoServer
  threadDelay 1000000 -- wait one second
  sendMessage "Hello, world!"
  threadDelay 1000000 -- wait one second
\end{code}

When `main` terminates all of the other threads in the program will terminate as well [[2]](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html#g:12).

A real world TCP server will likely need to constantly receive requests and 
make responses. We can make the request/response code an infinite loop.

\begin{code}
runTCPEchoServerForever :: IO ()
runTCPEchoServerForever = do 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  (conn, _) <- accept sock
  print "TCP server is waiting for a message..."
  rrLoop conn
  print "TCP server socket is closing now."
  close conn
  close sock
  
  where
    rrLoop conn = do
      msg <- recv conn 1024      
      unless (BS.null msg) $ do 
        print ("TCP server received: " ++ C.unpack msg)
        print "TCP server is now sending a message to the client"
        sendAll conn msg            
\end{code}

=== References

- [[1] Hackage :: network](https://hackage.haskell.org/package/network)

- [[2] Hackage :: base :: Control.Concurrent :: Terminating the program](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html#g:12)