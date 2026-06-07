---
title: Baby's First Effects with Haskell Effectful
kind: tutorial
state: complete
tags: haskell, effects
---

In this tutorial, we are going to build a simple file read and write function and a log function to explore how to use effects in Haskell.

For much of my work with Haskell, I've used the [ReaderT Design Pattern](https://academy.fpblock.com/blog/2017/06/readert-design-pattern/) to pass around configs, mutable references, database connections, etc. to different parts of the executable. It's a nice, simple pattern and good for smaller exectuables. It is still something I will use, but for larger projects, it is nice to have stricter control over what can happen in certain functions and encode that in the types.

That's where the effects come in. The idea is to encode the effects in a function's type signature and allow it to perform actions like reading or writing a file. [effectful](https://hackage-content.haskell.org/package/effectful) is a nice Haskell library for effects. The author has included a tutorial at [effectful](https://hackage-content.haskell.org/package/effectful-core-2.6.1.0/docs/Effectful-Dispatch-Dynamic.html) for dynamic effects. It is a nice start, but missing all the pieces for a compilable program so I want to fill in the gaps with this tutorial.

Let's start by setting up the GHC language extensions and imports we are going to use. 

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
\end{code}

Then we define a system of effects as data constructors. Dynamic means the effect system can have multiple interpretations.

\begin{code}
data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic
\end{code}

With the use of `send`, we can turn the data constructors into functions. `FileSystem` is an effect required by the function. `:>` means `FileSystem` is one of the effect types in the stack of effects `es`.

\begin{code}
readFile' :: (FileSystem :> es) => FilePath -> Eff es String
readFile' path = send (ReadFile path)

writeFile' :: (FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile' path contents = send (WriteFile path contents)
\end{code}

We define an interpreter for the system of effects. This gives an IO operation to each effect in the system. Start by looking at the type signature. `IOE` allows an arbitrary MonadIO computation. `Error` is an error effect we use with our custom error `FsError`. It allows this function to fail with `FsError`. `interpret` implements the effect and lets us implement each path in `FileSystem`.

\begin{code}
newtype FsError = FsError String deriving Show

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

Then we write a simple function that requires the FileSystem of effects and writes and reads from a text file.

\begin{code}
writeAndReadExampleFile :: (FileSystem :> es) => Eff es String
writeAndReadExampleFile = do
  writeFile' "/tmp/effectful-example.txt" "Hello from Effectful!\n"
  readFile' "/tmp/effectful-example.txt"
\end{code}

Use functions `runEff`, `runError` with our interpreter `runFileSystemIO` and the function that has the `FileSystem` effect in the type signature, `writeAndReadExampleFile`. If you run this in `main`, you will see it write a file, then reads the data from file and try to print it to stdout.

\begin{code}
testMain :: IO ()
testMain = do
  putStrLn "== runFileSystemIO (real disk) =="
  ioResult <- runEff . runError @FsError . runFileSystemIO $ writeAndReadExampleFile
  report ioResult
  where
    report res = case res of
      Left (callStack, FsError err) ->
        putStrLn $ "File system error: " <> err <> "\n" <> prettyCallStack callStack
      Right contents ->
        putStr $ "Read back:\n" <> contents
\end{code}

Now we define a second interpreter for the same effect. This treats the file system as a pure Map data structure. `reinterpret` allows us to run an internal effect that does not exist outside of this function. In this case it is a `State` effect.

\begin{code}
runFileSystemPure
  :: (Error FsError :> es)
  => Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es (a, Map FilePath String)
runFileSystemPure fs0 = reinterpret (runState fs0) $ \_ eff ->
  case eff of
    ReadFile path -> do
      fs <- get
      case Map.lookup path fs of
        Just contents -> pure contents
        Nothing       -> throwError . FsError $ "no such file: " <> path
    WriteFile path contents -> modify (Map.insert path contents)
\end{code}

And an IO function for running the pure effect in main.

\begin{code}
testPureEffect :: IO ()
testPureEffect = do
  putStrLn "\n== runFileSystemPure (in-memory) =="
  let pureResult = runPureEff . runError @FsError . runFileSystemPure Map.empty $ writeAndReadExampleFile
  report (fmap fst pureResult)
  where
    report res = case res of
      Left (callStack, FsError err) ->
        putStrLn $ "File system error: " <> err <> "\n" <> prettyCallStack callStack
      Right contents ->
        putStr $ "Read back:\n" <> contents
\end{code}

In order to show what error messages look like when you include an effect that is not included in the type signature, we will create a second effect system. Following the patterns from above, this should be pretty straightforward. Define the constructor, make the system dynamic, add a function for the constructor, then create an interpreter function.

\begin{code}
data Logger :: Effect where
  LogMsg :: String -> Logger m ()

type instance DispatchOf Logger = Dynamic

logMsg :: (Logger :> es) => String -> Eff es ()
logMsg msg = send (LogMsg msg)

runLoggerIO :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runLoggerIO = interpret $ \_ (LogMsg msg) -> liftIO (putStrLn ("[log] " <> msg))
\end{code}

\begin{code}
-- uncomment this code to see compiler error
-- this cannot compile because Logger is not part of the type signature
-- writeAndReadExampleFileBroken :: (FileSystem :> es) => Eff es String
-- writeAndReadExampleFileBroken = do
--   writeFile' "/tmp/effectful-example.txt" "Hello from Effectful with FileSystem and Logger!\n"
--   result <- readFile' "/tmp/effectful-example.txt"
--   logMsg result
--   pure result
\end{code}

In order to make it compile, we need to add Logger to the type signature.

\begin{code}
writeReadLogExampleFile :: (FileSystem :> es, Logger :> es) => Eff es String
writeReadLogExampleFile = do
  writeFile' "/tmp/effectful-example2.txt" "Hello from Effectful with FileSystem and Logger!\n"
  result <- readFile' "/tmp/effectful-example2.txt"
  logMsg result
  pure result

\end{code}

\begin{code}
testLoggerEffect :: IO ()
testLoggerEffect = do
  putStrLn "\n== runFileSystemIO + runLoggerIO =="
  result <- runEff . runError @FsError . runFileSystemIO . runLoggerIO $ writeReadLogExampleFile
  case result of
    Left (callStack, FsError err) ->
      putStrLn $ "File system error: " <> err <> "\n" <> prettyCallStack callStack
    Right contents ->
      putStr $ "Read back:\n" <> contents

main :: IO ()
main = do
  testMain
  testPureEffect
  testLoggerEffect
\end{code}

Try running compiling and running this code locally. You can find the source code [here](https://github.com/mchaver/mchaver.com/tree/master/posts/2026-06-3-babys-first-effects-with-haskell-effectful.lhs).
