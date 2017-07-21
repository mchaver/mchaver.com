---
title: Haskell Network Programming - UDP Client and Server
tags: haskell, networking
---

Using the [network](https://hackage.haskell.org/package/network) package we can 
build a low level UDP server and client. 

\begin{code}
import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

runUDPServer :: IO ()
runUDPServer = do 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
\end{code}

It is important to remember to use `Datagram` to receive data via UDP. Then we
can bind the socket to the address and wait to receive data.

\begin{code}
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print "UDP server is waiting..."
  recv sock 4096 >>= \message -> print ("UDP server received: " ++ (C.unpack message))
  print "UDP server socket is closing now."
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
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack s
  close sock
\end{code}

We run the server in a separate thread because `recv` is blocking. We add a few
`threadDelay`s to make sure that the server has started up and that the `print`s
from different threads do not occur at the same time. Otherwise, the messages 
might print at the same time and be illegible. 

\begin{code}
main :: IO ()
main = do
  _ <- forkIO $ runUDPServer
  threadDelay 1000000 -- wait one second
  sendMessage "Hello, world!"
  threadDelay 1000000 -- wait one second
\end{code}

When `main` terminates all of the other threads in the program will terminate as well [[2]](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html#g:12).

A real world UDP server will likely need to constantly receive requests. We can 
change the last line to use `forever`. It will repeatedly process incoming 
data.

\begin{code}
runUDPServerForever :: IO ()
runUDPServerForever = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos 
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  forever (do recv sock 4096 >>= print)
\end{code}

=== References

- [[1] Hackage :: network](https://hackage.haskell.org/package/network)

- [[2] Hackage :: base :: Control.Concurrent :: Terminating the program](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent.html#g:12)
