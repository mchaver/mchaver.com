---
title: Change Making Problem in Haskell
tags: haskell, dynamic programming
---


The [Change-making problem](https://en.wikipedia.org/wiki/Change-making_problem) asks us to find the minimum number of coins that add up to a target value. For example, if we have an infinite amount of coins of values `1, 2, 5`, the smallest set that adds up to `11` is `1, 5, 5`, whereas 11 `1` value coins would be the largest. Here is an outline of the recursive algorithm in imperative form (I will to convert this to real Python at some point):


```
coins = unique list of numbers >= 0
coin_index = length of coins
target = the value the sum of selected coins must sum, positive value

count(solution, coins, coin_index, target):
    # solution has been found
    if (target == 0):
        return [solution];

    # solution does not exist
    if (target < 0):
        return [];

    # no more coins to iterate over, but target has not been reached
    # this solution does not exist
    if (coin_index <= 0 && target >= 1):
        return [];

    # the left branch tries a different coin
    # the right branch uses the same coin, adds the coin to the solution, and
    # deducts the coins value from the target
    return
      append(
        count( solutions, coins, coin_index - 1, target)
             , count(append(solutions, coins[coin_index-1]), coins, coin_index, target - coins[coin_index - 1])
             )
        );

return get_small_list(count([], coins, coin_index, target));
```


The translation to Haskell is pretty straightforward. I then use a fold to get the smallest length solution. The results from `makeChangesSolutions` can also be used to answer how many solutions are there. You can also filter the results to answer other things like which solutions have a certain amount coin, etc.

\ignore{
\begin{code}
import qualified Data.Map as Map
\end{code}
}

\begin{code}
makeChangeSolutions :: [Int] -> Int -> [[Int]]
makeChangeSolutions coins target = makeChange' [] coins (length coins) target
  where
    makeChange' :: [Int] -> [Int] -> Int -> Int -> [[Int]]
    makeChange' coinSet coins coinIndex target
      | target <  0 = []
      | target == 0 = [coinSet]
      | coinIndex == 0 && target >= 1 = []
      | otherwise = (makeChange' coinSet coins (coinIndex - 1) target) ++ (makeChange' (coinSet  ++ [coins !! (coinIndex - 1)]) coins coinIndex (target - (coins !! (coinIndex - 1))))

minimumChangeSolution :: [Int] -> Int -> [Int]
minimumChangeSolution coins target =
  if length solutions == 0 then [] else foldl (\a b -> if length a < length b then a else b) (head solutions) (tail solutions)
  where
    solutions = makeChangeSolutions coins target
\end{code}

Both these implementations are brute force though and they repeat calculations that we could save and lookup instead of recalculating them.

The dynamic programming implementation was quite challenging because I want to return not just the amount of solutions are the number of coins in the solution, but the actual soltion.
Returning the former two is not too hard with some table lookups, but the latter is more challenging. I ended up referring to a Python implementation in the [Problem Solving with Algorithms and Data Structures](https://interactivepython.org/courselib/static/pythonds/Recursion/DynamicProgramming.html). That looks like this:

```python
def dpMakeChange(coinValueList,change,minCoins,coinsUsed):
   for cents in range(change+1):
      coinCount = cents
      newCoin = 1
      for j in [c for c in coinValueList if c <= cents]:
            if minCoins[cents-j] + 1 < coinCount:
               coinCount = minCoins[cents-j]+1
               newCoin = j
      minCoins[cents] = coinCount
      coinsUsed[cents] = newCoin
   return minCoins[change]

def printCoins(coinsUsed,change):
   coin = change
   while coin > 0:
      thisCoin = coinsUsed[coin]
      print(thisCoin)
      coin = coin - thisCoin
```

My Haskell implementation is quite complex. I ended up using two `foldl` which probably is not good for performance. I owe you an explanation of the code.

\begin{code}
initMap :: Int -> Map.Map Int Int
initMap x = Map.fromList $ zip [0..x] (replicate (x+1) 0)

dpMinimumChangeSolution :: [Int] -> Int -> [Int]
dpMinimumChangeSolution coinValues change = filterUsedCoins coinsUsedMap change []
  where
    (_, coinsUsedMap) =
      foldl (\(minCoins, coinsUsed) cents ->
        let (coinCount', newCoin') = foldl (\(coinCount, newCoin) j ->
             let (coinCount', newCoin') =
                  case Map.lookup (cents - j) minCoins of
                    Just res -> 
                      if (res + 1 < coinCount)
                        then (res + 1, j)
                        else (coinCount, newCoin)
                    Nothing -> (coinCount, newCoin)
             in (coinCount', newCoin')
             ) (cents, 1) (filter (\c -> c <= cents) coinValues)
        in (Map.update (\_ -> Just coinCount') cents minCoins, Map.update (\_ -> Just newCoin') cents coinsUsed)
      ) (initMap change, initMap change) [0..change]

    filterUsedCoins :: Map.Map Int Int -> Int -> [Int] -> [Int]
    filterUsedCoins coinsUsed change res
      | change > 0 =
          case Map.lookup change coinsUsed of
            Just coin -> filterUsedCoins coinsUsed (change - coin) (res ++ [coin])
            Nothing   -> res -- this shouldn't happen
      | otherwise  = res
\end{code}

Finally the tests.

\begin{code}
test  = [1,2,3]
test2 = [1,5,10,21,25]

main :: IO ()
main = do
  print $ minimumChangeSolution   test 4 -- [1,3]
  print $ dpMinimumChangeSolution test 4 -- [1,3]

  print $ minimumChangeSolution   test2 63 -- [21,21,21]
  print $ dpMinimumChangeSolution test2 63 -- [21,21,21]
\end{code}
