---
title: Trees in Haskell Part 1: Binary Trees 
tags: haskell, trees, binary-tree, 99 problems
---

== Type declaration

Binary trees are a common data structure in the study of algorithms and useful for getting comfortable with new programming languages. To limit the scope of this post, we will only define a binary tree for `Int` instead of one that accepts a type parameter because to do it right would require using the `GADTs` extensions to enable the `Ord` and `Eq` restrictions on a the type parameter.

\begin{code}
import Data.List (group, sort)

data Tree = Nil | Node Tree Int Tree deriving (Eq, Show)
\end{code}

This is a simple recursive data structure in Haskell. Each `Node` constructor takes a left-side `Tree`, a node value, and a right-side `Tree`, `Nil` ends the recursion. A simple single `Node` tree with a value of `1` is defined as `Node Nil 1 Nil`. A three node tree is `Node (Noe Nil 2 Nil) 1 (Node Nil 3 Nil)`. This tree looks like this:

\begin{code}
{-
    1
   / \
  2   3
-}
\end{code}

== Traversal functions

What would it look like to map a function over each `Int` value for our binary tree?

\begin{code}
treeMap :: (Int -> Int) -> Tree -> Tree
treeMap _ Nil = Nil
treeMap f (Node l v r) = (Node (treeMap f l) (f v) (treeMap f r))
\end{code}

If it is `Nil`, you just return `Nil` and if it is a `Node`, apply the function `f` to the value, and then call the map function on the left and right children of the `Node`. If you change `Int` to `a` then you can easily turn this into the `Functor` definition for the binary tree; change the type signature of `f` to `(a -> a)`.

The various depth first traversal functions, pre-order (NLR), in-order (LNR) and post-order (LRN), are defined similarly to the map funcion above.

\begin{code}
depthFirstPreOrder :: Tree -> [Int]
depthFirstPreOrder Nil = []
depthFirstPreOrder (Node l v r) = [v] ++ (depthFirstPreOrder l) ++ (depthFirstPreOrder l)

depthFirstInOrder :: Tree -> [Int]
depthFirstInOrder (Nil) = []
depthFirstInOrder (Node l v r) = (depthFirstInOrder l) ++ [v] ++ (depthFirstInOrder r)

depthFirstPostOrder :: Tree -> [Int]
depthFirstPostOrder (Nil) = []
depthFirstPostOrder (Node l v r) = (depthFirstPostOrder l) ++ (depthFirstPostOrder r) ++ [v]
\end{code}

Breadth first traversal is more complex. The strategy is to build a queue and traverse it at the same time. When the `Node` is `Nil`, it continues to traverse, when the `Node` has a value, it adds the value to results list, and add the left and right children of the current node to the end of the queue. When the queue is empty, the results are returned, then it converts `Node`s to `Just x`, `Nil` to `Nothing`, then uses `catMaybes` to turn it into `[Int]`.

\begin{code}
breadthFirst :: Tree -> [Int]
breadthFirst tree = catMaybes $ nodeVal <$> breadthFirst' [tree] [tree]
  where
    breadthFirst' :: [Tree] -> [Tree] -> [Tree]
    breadthFirst' (Nil : queue) orderedNodes = breadthFirst' queue orderedNodes
    breadthFirst' (Node l _ r : queue) orderedNodes = breadthFirst' (queue ++ [l,r]) (orderedNodes ++ [l,r])
    breadthFirst' _ orderedNodes = orderedNodes

    nodeVal :: Tree -> Maybe Int
    nodeVal Nil = Nothing
    nodeVal (Node _ x _) = Just x
\end{code}


== Balanced Binary Search Tree

If we want to use the binary tree as a binary search tree, each value must be a unique key, and the keys in each node are greater than or equal to the keys its left subtree and less than or equal to the keys in its right subtree. We start with a simple insert function that traverses the tree the same way a search does, but it may imbalance the tree (the height difference between one or more branches is greater than one).

\begin{code}
insert :: Tree -> Int -> Tree
insert Nil insertVal = Node Nil insertVal Nil
insert (Node l currentNodeVal r) insertVal
  | insertVal == currentNodeVal = Node l currentNodeVal r
  | insertVal  < currentNodeVal = Node (insert l insertVal) currentNodeVal r
  | otherwise                   = Node l currentNodeVal (insert r insertVal)
\end{code}

To make a balanced binary search tree, our strategy is to transform a list of values into a list of unique values that is sorted such that `insert` can be folded over it to create a binary search tree with values in the correct order to make it balanced. Given a list, repeteadly take the middle value, in case of even length the middle plus one, move it to the front, and repeat the process on the remaining values to the left and right of the middle value. 

\begin{code}
mkBalancedBinarySearchTreeList :: (Ord a) => [a] -> [a]
mkBalancedBinarySearchTreeList = mkBalancedBinarySearchTreeList' . sortAndRmdups
  where
    sortAndRmdups = map head . group . sort

    mkBalancedBinarySearchTreeList' :: [a] -> [a]
    mkBalancedBinarySearchTreeList' [] = []
    mkBalancedBinarySearchTreeList' xs =
      case length xs > 2 of
        True ->
          let mid = (quot (length xs) 2)
              (left, right) = splitAt mid xs
              (rightL, rightR) = splitAt 1 right
          in rightL ++ mkBalancedBinarySearchTreeList' left ++ mkBalancedBinarySearchTreeList' rightR
        False -> xs

mkTree :: [Int] -> Tree
mkTree xs = foldl insert Nil xs

mkBinaryBalancedSearchTree :: [Int] -> Tree
mkBinaryBalancedSearchTree = mkTree . mkBalancedBinarySearchTreeList
\end{code}

Now to see if a binary search tree contains a value, traverse across the tree comparing the value, if it reaches a `Node` that has a matching value, then it is `True`, if it reaches a `Nil` node, then it does not exist in the tree.

\begin{code}
contains :: Tree -> Int -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
  | x == v    = True
  | x  < v    = contains t1 x 
  | otherwise = contains t2 x
\end{code}

Delete is the most complicated function so far.

\begin{code}
leftMost :: Tree -> Maybe Int
leftMost Nil = Nothing
leftMost (Node Nil v _) = Just v
leftMost (Node left v _) = leftMost left

rightMost :: Tree -> Maybe Int
rightMost Nil = Nothing
rightMost (Node _ v Nil) = Just v
rightMost (Node _ v r) = rightMost r

delete :: Tree -> Int -> Tree
delete Nil _ = Nil
delete (Node l v r) x
  | x == v   =
      case (succ l r) of
        Just succ' -> Node (left l r) succ' (right l r)
        Nothing -> Nil
  | x <  v   = Node (delete l x) v r
  | x >  v   = Node  l v (delete r x)
  where
    succ :: Tree -> Tree -> Maybe Int
    succ l Nil = rightMost l
    succ _ r   = leftMost r

    left :: Tree -> Tree -> Tree
    left l Nil = leftSubtree l
    left l _   = l

    right :: Tree -> Tree -> Tree
    right l Nil = Nil
    right _ r   = rightSubtree r

    -- return the right subtree without the leftmost item
    rightSubtree :: Tree -> Tree
    rightSubtree Nil = Nil
    rightSubtree (Node Nil _ r) = r
    rightSubtree (Node l v r) = Node (rightSubtree l) v r
    -- return the left subtree without the rightmost item
    leftSubtree :: Tree -> Tree
    leftSubtree Nil = Nil
    leftSubtree (Node l _ Nil) = l
    leftSubtree (Node l v r) = Node l v $ leftSubtree r


\end{code}

delete (Node (Node Nil 1 Nil) 2 (Node (Node Nil 4 Nil) 3 Nil)) 1
delete (Node (Node Nil 1 Nil) 2 (Node (Node Nil 4 Nil) 3 Nil)) 2
delete (Node (Node Nil 1 Nil) 2 (Node (Node Nil 4 Nil) 3 Nil)) 3
delete (Node (Node Nil 1 Nil) 2 (Node (Node Nil 4 Nil) 3 Nil)) 4



\ignore{
\begin{code}
mkTree :: [Int] -> Tree
mkTree []    = Nil
mkTree (h:t) =
  let mkTree' tr [] = tr
      mkTree' tr (h:t) = mkTree' (insert tr h) t
  in  mkTree' (Node Nil h Nil) t
\end{code}

\begin{code}
contains :: Tree -> Int -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
  | x == v    = True
  | x  < v    = contains t1 x 
  | otherwise = contains t2 x
\end{code}


\begin{code}
-- problem 55
mkBalanceTreeOfLayers :: Int -> Tree
mkBalanceTreeOfLayers n
  | n < 1 = Nil
  | otherwise = Node (mkBalanceTreeOfLayers $ n - 1) 1 (mkBalanceTreeOfLayers $ n - 1)



countBranches :: Tree -> Int
countBranches Nil = 0
countBranches (Node l _ r) = 1 + countBranches l + countBranches r

-- isBalancedTree :: Tree -> Bool
-- isBalancedTree Nil = True
-- isBalancedTree (Node l _ r) =
--   abs (countBranches l - countBranches r) <= 1
--   && isBalancedTree l && isBalancedTree r

\end{code}

\begin{code}
-- problem 56
mirror :: Tree -> Tree -> Bool
mirror Nil Nil = True
mirror (Node l1 _ r1) (Node l2 _ r2) = mirror l1 r2 && mirror l2 r1
mirror _ _ = False

symmetric :: Tree -> Bool
symmetric t = mirror t t
\end{code}


\begin{code}
-- problem 57
add :: Int -> Tree -> Tree
add x Nil = Node Nil x Nil
add x n@(Node l v r) =
  case compare x v of
    LT -> Node (add x l) v r
    GT -> Node l v (add x r)
    EQ -> n

construct :: [Int] -> Tree
construct xs = foldl (flip add) Nil xs

\end{code}


\begin{code}
-- problem 58
reverseTree Nil = Nil
reverseTree (Node l x r) = Node (reverseTree r) x (reverseTree l)
\end{code}

\begin{code}
-- problem 61
countLeaves :: Tree -> Int
countLeaves Nil = 0
countLeaves (Node Nil _ Nil) = 1
countLeaves (Node l _ r) = countLeaves l + countLeaves r
\end{code}


\begin{code}
internals :: Tree -> [Int]
internals Nil                = []
internals (Node Nil a Nil) = []
internals (Node left a right ) = a : internals left ++ internals right
\end{code}
}
