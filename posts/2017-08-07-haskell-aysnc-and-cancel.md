---
title: Haskell async and cancel
tags: haskell, async
---

I was asked to help debug the following code. It's a simple example meant to be
added to a bigger system. There are multiple asynchronous bits of code that 
update a value, but only one should be completed, furthermore a user event can 
override the other events. I was not clear on all the details, but the code I 
was given looked like this.


```haskell
module Main where
  
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

main = do
  continuous <- async $ do
    threadDelay $ (6 * 10 ^ (6 :: Int))
    print ("continuous" :: String)

  userEvent <- async $ do
    print ("userEvent" :: String)
    uninterruptibleCancel continuous

  print ("pre-waitAnyCancel" :: String)
  _ <- waitAnyCancel [continuous, userEvent]
  print ("post-waitAnyCancel" :: String)
```

The output looks like this.

```
$> stack runghc Main.hs
"userE"vpernet-"w
aitAnyCancel"
Main.hs: thread killed
```

The mixed output from multiple threads is normal, but I was not expecting 
`Main.hs: thread killed`. After trying out various functions from the `async` 
library, I was able to make it run with 
`_ <- waitAnyCatchCancel [continuous, userEvent]`. This cancels the other 
operations, but it also catches any errors. 

We did not realize that `cancel` works by throwing an error. That is what caused
the first version to crash. The Hackage  documentation states it clearly:

```
Cancel an asynchronous action by throwing the ThreadKilled exception to it, and 
waiting for the Async thread to quit.
```

## References

- [Hackage :: async](https://hackage.haskell.org/package/async-2.1.1.1/docs/Control-Concurrent-Async.html)
