---
title: Maximum Subarray in Haskell
tags: haskell, dynamic programming
---

[Dynamic programming](https://en.wikipedia.org/wiki/Dynamic_programming) is an algorithmic technique that trades space for improved runtime performance and takes advantage of overlapping subproblems. [Memoization](https://en.wikipedia.org/wiki/Memoization) is a common strategy within Dynamic programming; store the results of expensive computations and look them up when the same input occurs again. Most literature on algorithms provide an imperative programming solution instead of a declarative one. I want to work through some dynamic programming problems with Haskell.

The [maximum subarray problem](https://en.wikipedia.org/wiki/Maximum_subarray_problem) is an easy place to start. Given a list of numbers, what is the subarray (contiguous) that sums to the largest value. If the array only includes positive values, then it is the entire subarray. If it includes negative values, then the answer is most likely a subarray. Kadane's algorithm provides a solution in O(n) time. Here is the imperative form:

```
max_so_far = 0
max_ending_here = 0

For each element in the array a:
    max_ending_here = max_ending_here + a[i]
    if (max_ending_here < 0)
        max_ending_here = 0
    if (max_so_far < max_ending_here)
        max_so_far = max_ending_here

return max_so_far
```

The strategy is to store the max result of any subarray in `max_so_far` and when the loop ends, it contains the correct answer. `max_ending_here` contains the result from some index up to the current index. The beginning index of the subarray is reset if `max_ending_here` is less than zero, and `max_so_far` is updated only when it is less than `max_ending_here`.

The translation to Haskell is pretty simple. Instead of two mutable values, we create an internal function that takes the updated values of `maxSoFar` and `maxEndingHere` and call it recursively over the values of the list and when the list is empty, we return the `maxSoFar` value.

\begin{code}
simpleMaxSubarray :: [Int] -> Int
simpleMaxSubarray = simpleMaxSubarray' 0 0
  where
    simpleMaxSubarray' maxSoFar maxEndingHere []     = maxSoFar     
    simpleMaxSubarray' maxSoFar maxEndingHere (x:xs) = simpleMaxSubarray' maxSoFar' maxEndingHere' xs
      where
        maxEndingHere' = max x (maxEndingHere + x)
        maxSoFar'      = max maxSoFar maxEndingHere'
\end{code}

I decided to improve the function and return the start and end indexes of the max subarray.

\begin{code}
maxSubarray :: [Int] -> (Int, Int, Int)
maxSubarray = maxSubarray' 0 0 0 0 0
  where
    maxSubarray' maxSoFar maxEndingHere i start end []     = (maxSoFar, start, end)
    maxSubarray' maxSoFar maxEndingHere i start end (x:xs) =
      let i' = i + 1 
          maxEndingHere' = maxEndingHere + x
      in 
      if (maxSoFar < maxEndingHere')
        then maxSubarray' maxEndingHere' maxEndingHere' i' start i xs
        else
          if maxEndingHere' < 0
            -- reset the subarray we are checking
            then maxSubarray' maxSoFar 0              i' i'    i'  xs
            else maxSubarray' maxSoFar maxEndingHere' i' start end xs
\end{code}

You can see that the amount of values we need to pass to the function is increasing. This makes it harder to read and keep track of the values. In the future, for more complex algorithms we might consider some monadic tools like `Reader`, `Writer` and `State` to reduce the complexity. We will test `maxSubarray` below and take a look at some of the results.

\begin{code}
test  = [-2, -3, 4, -1, -2, 1, 5, -3]
test2 = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
test3 = [1,2,3,4,5]
test4 = [1,2,3,4,5, -5]

main :: IO ()
main = do
  print $ maxSubarray test  -- (7,  2, 6)
  print $ maxSubarray test2 -- (6,  3, 6)
  print $ maxSubarray test3 -- (15, 0, 4)
  print $ maxSubarray test4 -- (15, 0, 4)
\end{code}
