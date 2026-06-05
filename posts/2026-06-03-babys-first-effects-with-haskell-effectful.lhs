---
title: Baby's First Effects with Haskell Effectful
kind: tutorial
state: developing
tags: haskell, effects
---

`effectful` is a nice Haskell library for effects. The main advantage over a traditional Monad stack is it allows tha subset of effects that a particular function uses. That way if you try to use an effect that is not allow, it will throw an error.

https://academy.fpblock.com/blog/2017/06/readert-design-pattern/

The general idea is you define systemd of effects as data constructors. Then you define how those effects are interpreted. You can have multiple interpretations.

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Effectful (Dispatch(Dynamic), DispatchOf, Eff, Effect, IOE, (:>), liftIO, runEff, runPureEff)
import Effectful.Error.Static (Error, prettyCallStack, runError, throwError)
import Effectful.Exception (catchIO)
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.State.Static.Local (get, modify, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

newtype FsError = FsError String deriving Show

data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic
\end{code}

with the use of `send`, you turn the data constructors in functions.

\begin{code}
readFile' :: (FileSystem :> es) => FilePath -> Eff es String
readFile' path = send (ReadFile path)

writeFile' :: (FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile' path contents = send (WriteFile path contents)
\end{code}

then you define an interpreter for the system of effects. this gives an IO operation to each effect in the system

\begin{code}
runFileSystemIO
  :: (IOE :> es, Error FsError :> es)
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ eff ->
  case eff of
    ReadFile path           -> adapt $ IO.readFile path
    WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catchIO` \e -> throwError . FsError $ show e
\end{code}


then we can write a simple function that requires the FileSystem of effects and write and reads from a text
\begin{code}
program :: (FileSystem :> es) => Eff es String
program = do
  writeFile' "/tmp/effectful-example.txt" "Hello from Effectful!\n"
  readFile' "/tmp/effectful-example.txt"
\end{code}

now we can combine the interpreter of an effect system, with a function that expects that affect sysem

\begin{code}

testMain :: IO ()
testMain =
  putStrLn "== runFileSystemIO (real disk) =="
  ioResult <- runEff . runError @FsError . runFileSystemIO $ program
  report ioResult
  where
    report res = case res of
      Left (callStack, FsError err) ->
        putStrLn $ "File system error: " <> err <> "\n" <> prettyCallStack callStack
      Right contents ->
        putStr $ "Read back:\n" <> contents

\end{code}
