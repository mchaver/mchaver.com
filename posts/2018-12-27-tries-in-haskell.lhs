---
title: Tries in Haskell
tags: haskell, tree
---

A trie is a nice tree data structure for storing items we want to frequently look up. It can be used to build a simple autocomplete or spell checking system. It stores sequences with the same prefixes (beginning sequence) together to reduce space and can performs a look up that takes `O(m)` where `m` is the length of the string we search.

Here is a simple trie dictionary to give you an idea of the structure (heat, heater, help, helper, hot, hotter, hottest). The `.` in the trie below shows where the words end. While we can see `he` in the structure, it does not have a `.` so it is not part of the dictionary, a lookup of `he` returns `False` and of `hot` returns `True`.

```
           h
      /    | 
     e     o
/    |     |
a    l     t.
|    |     |
t.   p.    t
|    |     |
e    e     e
|    |     | \
r.   r.    r. s
              |
              t.
```

== Type declaration

A trie is a recursive structure. At each level it has a `Bool` representing a potential sequence boundary and stores a `Map` of `Trie`s. The sequence boundaries have the `Bool` set to `True` and the leaf nodes have `True` and an empty map.

\ignore{
\begin{code}
import Test.Hspec
\end{code}
}

\begin{code}
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)

data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Eq, Read, Show)
\end{code}

An empty trie is simple.

\begin{code}
empty :: Trie a
empty = Trie False Map.empty
\end{code}

== The insert function

To insert and update values into a trie, we will use [Map.alter](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:alter) `Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a`. Given a function `f` it will `insert`, `update` or `delete` a value `a` at key `k`. If the function returns `Just` it will `insert` (if `k` does not exist) or `update` it, if it returns `Nothing` than it will delete the value at `k` if it exists.

When the sequence is finished, we set the last node to `True` so that it knows there is a boundary when it looks up a sequence. While iterating throught the values of the sequence, it keeps the same `Bool` value, and recursively updates the child nodes at `x`.

\begin{code}
insert :: Ord a => [a] -> Trie a -> Trie a
insert []     (Trie _ nodes)     = Trie True nodes
insert (x:xs) (Trie end nodes) = Trie end (Map.alter (Just . insert xs . fromMaybe empty) x nodes)
\end{code}

Now we can build a simple dictionary with `insert` and a helper function.

\begin{code}
mkTrie :: Ord a => [[a]] -> Trie a
mkTrie as = mkTrie' as empty
  where
    mkTrie' []     trie = trie
    mkTrie' (x:xs) trie = mkTrie' xs $ insert x trie

dictionary = mkTrie ["bad", "good", "heat", "heater", "help", "helper", "hot", "hotter", "hottest", "p", "pi", "sad", "said"]
\end{code}

== The member function

We should check that we can find these words in the dictionary.

\begin{code}
member :: Ord a => [a] -> Trie a -> Bool
member []     (Trie end _) = end
member (x:xs) (Trie _ nodes) = fromMaybe False (member xs <$> Map.lookup x nodes)
\end{code}

These return `True`:

```
λ> member "heat" dictionary
λ> member "hot" dictionary
λ> member "helper" dictionary
```

And these return `False`:

```
λ> member "he" dictionary
λ> member "hello" dictionary
λ> member "goodbye" dictionary
```

== The delete function

This function gave me the most trouble because you need to keep track of which sequences are shared. For example, "help" and "helper" both share "help". When we delete "helper", we still need to keep "help". We need to consider the following patterns when deleting:

==== forked

```
  m
  |
  e
 / \
 t. e
    |
    t.
```

==== in-line

```
  h
  |
  e
  |
  a
  |
  t.
  |
  e
  |
  r.
```

==== separate paths

```
  g  b
  |  |
  o  a
  |  |
  o  d.
  |
  d.
```

The main idea is to start from the top of the trie and follow the sequence all the way down, if certain conditions are met, then we can delete the sequence, if not, we continue through the subsequences checking if we can delete anything. In case we want to delete a boundary that is not at a leaf, we the list is empty, we check if there are any children, if there are we set it to `False`. It took a bit of trial, error and unit testing to write these functions so I will not claim that they are correct yet.

\begin{code}
lengthOfChildNodes :: Trie a -> Int
lengthOfChildNodes (Trie _ nodes) = Map.size nodes

deletable :: Ord a => [a] -> Trie a -> Bool
deletable []       (Trie _ nodes) = Map.null nodes
deletable (x : xs) (Trie end nodes) =
  (length xs == 0 || not end) &&
  maybe False (\t -> deletable xs t && (length xs == 0 || (lengthOfChildNodes t) < 1)) (Map.lookup x nodes)

delete :: Ord a => [a] -> Trie a -> Trie a
delete as t = if member as t then delete' as t else t
  where
    delete' as@(x : xs) t@(Trie end nodes) =
      if deletable as t
        then Trie end (Map.delete x nodes)
        else Trie end (Map.alter (Just . delete' xs . fromMaybe empty) x nodes)
    delete' [] t@(Trie end nodes) = if Map.size nodes > 0 then Trie False nodes else t
\end{code}

The `delete` function is slow because it repeatedly traces over the same path as it gets smaller until it is deleted. If you need to perform delete a lot on a trie, one idea is to keep track of what paths are deletable with and extra boolean flag on each level. You would need to update the `insert` function to support this.

== Testing

Here are the tests I used to develop the functions.

\begin{code}
main :: IO ()
main = hspec $
  describe "delete" $ do
    it "forked" $ do
      (member "hottest" $ delete "hotter"  $ dictionary) `shouldBe` True
      (member "hotter"  $ delete "hottest" $ dictionary) `shouldBe` True
      (member "hottest" $ delete "hottest" $ dictionary) `shouldBe` False
      (member "hotter"  $ delete "hotter"  $ dictionary) `shouldBe` False

    it "inline" $ do
      (member "help"   $ delete "helper" $ dictionary) `shouldBe` True
      (member "helper" $ delete "help"   $ dictionary) `shouldBe` True
      (member "sad"    $ delete "said"   $ dictionary) `shouldBe` True
      (member "said"   $ delete "sad"    $ dictionary) `shouldBe` True
      (member "p"      $ delete "pi"     $ dictionary) `shouldBe` True
      (member "pi"     $ delete "p"      $ dictionary) `shouldBe` True
      (member "help"   $ delete "help"   $ dictionary) `shouldBe` False
      (member "helper" $ delete "helper" $ dictionary) `shouldBe` False
      (member "heat"   $ delete "heat"   $ dictionary) `shouldBe` False
      (member "sad"    $ delete "sad"    $ dictionary) `shouldBe` False
      (member "said"   $ delete "said"   $ dictionary) `shouldBe` False
      (member "p"      $ delete "p"      $ dictionary) `shouldBe` False
      (member "pi"     $ delete "pi"     $ dictionary) `shouldBe` False

    it "separate paths" $ do
      (member "bad"  $ delete "good" $ dictionary) `shouldBe` True
      (member "good" $ delete "bad"  $ dictionary) `shouldBe` True
      (member "bad"  $ delete "bad"  $ dictionary) `shouldBe` False
      (member "good" $ delete "good" $ dictionary) `shouldBe` False
\end{code}

