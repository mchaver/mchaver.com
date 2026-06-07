---
title: Baby's First Effects with Haskell Effectful
kind: tutorial
state: complete
tags: haskell, effects
---

In this tutorial, we are going to build a simple file read and write function and a log function to explore how to use effects in Haskell.

For much of my work with Haskell, I've used the [ReaderT Design Pattern](https://academy.fpblock.com/blog/2017/06/readert-design-pattern/) to pass around configs, mutable references, database connections, etc. to different parts of the executable. It's a nice, simple pattern and good for smaller executables. It is still something I will use, but for larger projects, it is nice to have stricter control over what can happen in certain functions and encode that in the types.

That's where the effects come in. The idea is to encode the effects in a function's type signature and allow it to perform actions like reading or writing a file. [effectful](https://hackage-content.haskell.org/package/effectful) is a nice Haskell library for effects. The author has written a [document](https://hackage-content.haskell.org/package/effectful-core-2.6.1.0/docs/Effectful-Dispatch-Dynamic.html) for dynamic effects. However, it is not a complete tutorial. I want to fill in the gaps with this tutorial to give the reader a compilable program.

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

Then we define a system of effects as data constructors. The type family instance `type instance DispatchOf FileSystem = Dynamic` marks `FileSystem` as dynamically dispatched, which means we can give it more than one interpretation at run time. We will give it two interpretations in this tutorial.

\begin{code}
data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic
\end{code}

With the use of `send`, we can turn the data constructors into functions. `send` hands an operation off to whichever interpreter is installed for `FileSystem` when the program runs. `FileSystem` is an effect required by the function. `:>` means `FileSystem` is one of the effect types in the stack of effects `es`.

\begin{code}
readFile' :: (FileSystem :> es) => FilePath -> Eff es String
readFile' path = send (ReadFile path)

writeFile' :: (FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile' path contents = send (WriteFile path contents)
\end{code}

We define an interpreter for the system of effects. This gives an IO operation to each effect in the system. Start by looking at the type signature. `IOE` allows an arbitrary MonadIO computation. `Error` is an error effect we use with our custom error `FsError`. It allows this function to fail with `FsError`. `interpret` implements the effect and lets us implement each path in `FileSystem`. Each path goes through `adapt`, which runs the IO action and converts any `IOException` into our typed `FsError` with `throwError`, so failures are returned through the `Error` effect instead of as untyped runtime exceptions.

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

We run the effect by taking off one interpreter at a time. `runFileSystemIO` executes the `FileSystem` effect, `runError @FsError` executes the `Error` effect, and `runEff` executes `IOE` to get back to plain `IO`. The `@FsError` type application tells `runError` which error type to handle, since it would otherwise be ambiguous. `runError` turns a computation into `Either (CallStack, FsError) a`: a `Right` on success, or a `Left` carrying the error along with the call stack from where it was thrown. We pattern match on that in `report`, using `prettyCallStack` to render the stack as readable text. If you run this in `main`, it writes a file, reads the data back, and prints it to stdout.

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

Now we define a second interpreter for the same effect. This treats the file system as a pure Map data structure. `reinterpret` allows us to run an internal effect that does not exist outside of this function. In this case it is a `State` effect, which is what we need to thread the in-memory `Map` through each read and write in place of the disk. Because the file system only shows up as an effect in the type signature, we can swap in this pure interpreter and run the exact same program with no IO at all. This makes effectful code easier to test.

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

Uncommenting `writeAndReadExampleFileBroken` and compiling produces the following error. GHC notices that `logMsg` needs `Logger :> es`, but the type signature only promises `FileSystem :> es`, so it refuses to compile.

```
Main.lhs:193:3: error: [GHC-39999]
    • Could not deduce ‘Logger :> es’ arising from a use of ‘logMsg’
      from the context: FileSystem :> es
        bound by the type signature for:
                   writeAndReadExampleFileBroken :: forall (es :: [Effect]).
                                                    (FileSystem :> es) =>
                                                    Eff es String
        at Main.lhs:189:1-68
    • In a stmt of a 'do' block: logMsg result
      In the expression:
        do writeFile'
             "/tmp/effectful-example.txt"
             "Hello from Effectful with FileSystem and Logger!\n"
           result <- readFile' "/tmp/effectful-example.txt"
           logMsg result
           pure result
      In an equation for ‘writeAndReadExampleFileBroken’:
          writeAndReadExampleFileBroken
            = do writeFile'
                   "/tmp/effectful-example.txt"
                   "Hello from Effectful with FileSystem and Logger!\n"
                 result <- readFile' "/tmp/effectful-example.txt"
                 logMsg result
                 ....
    |
193 |   logMsg result
    |   ^^^^^^
```

In order to make it compile, we need to add Logger to the type signature.

\begin{code}
writeReadLogExampleFile :: (FileSystem :> es, Logger :> es) => Eff es String
writeReadLogExampleFile = do
  writeFile' "/tmp/effectful-example2.txt" "Hello from Effectful with FileSystem and Logger!\n"
  result <- readFile' "/tmp/effectful-example2.txt"
  logMsg result
  pure result
\end{code}

Now this function needs two effects, so we run it through two interpreters: `runLoggerIO` and `runFileSystemIO`, ahead of `runError` and `runEff`. Each one removes its effect from the stack until it reaches `IO`.

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
\end{code}

`main` ties the three examples together.

\begin{code}
main :: IO ()
main = do
  testMain
  testPureEffect
  testLoggerEffect
\end{code}

Running the program prints the following:

```
== runFileSystemIO (real disk) ==
Read back:
Hello from Effectful!

== runFileSystemPure (in-memory) ==
Read back:
Hello from Effectful!

== runFileSystemIO + runLoggerIO ==
[log] Hello from Effectful with FileSystem and Logger!

Read back:
Hello from Effectful with FileSystem and Logger!
```

Try compiling and running this code locally. You can find the source code [here](https://github.com/mchaver/mchaver.com/tree/master/posts/2026-06-03-babys-first-effects-with-haskell-effectful.lhs).
