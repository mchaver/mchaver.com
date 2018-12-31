---
title: Change Making Problem in Haskell
tags: haskell, dynamic programming
---


The (Change-making problem)[https://en.wikipedia.org/wiki/Change-making_problem] asks us to find the minimum number of coins that add up to a target value. For example, if we have an infinite amount of coins of value `1, 2, 5`, the smallest set that adds up to `11` is `1, 5, 5`, whereas 11 `1` value coins would be the largest. Here is an outline of the recursive algorithm in imperative form:


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

These algorithms are brute force though and they repeat calculations that we could save and lookup instead of recalculating them.

```
makeChange :: [Int] -> Int -> (Int, [[Int]])
makeChange coins target = makeChange' [] coins (length coins) target
  where
    add :: (Int, [[Int]]) -> (Int, [[Int]]) -> (Int, [[Int]])
    add (f,s) (f',s') = (f+f', s++s')

    makeChange' :: [Int] -> [Int] -> Int -> Int -> (Int, [[Int]])
    makeChange' coinSet coins coinIndex target
      | target <  0 = (0, [])
      | target == 0 = (1, [coinSet])
      | coinIndex == 0 && target >= 1 = (0, [])
      | otherwise = (makeChange' coinSet coins (coinIndex - 1) target) `add` (makeChange' (coinSet  ++ [coins !! (coinIndex - 1)]) coins coinIndex (target - (coins !! (coinIndex - 1))))

```

```
def count(S, m, n): 
    # We need n+1 rows as the table is constructed  
    # in bottom up manner using the base case 0 value 
    # case (n = 0) 
    table = [[0 for x in range(m)] for x in range(n+1)] 
  
    # Fill the entries for 0 value case (n = 0) 
    for i in range(m): 
        table[0][i] = 1
  
    # Fill rest of the table entries in bottom up manner 
    for i in range(1, n+1): 
        for j in range(m): 
  
            # Count of solutions including S[j] 
            x = table[i - S[j]][j] if i-S[j] >= 0 else 0
  
            # Count of solutions excluding S[j] 
            y = table[i][j-1] if j >= 1 else 0
  
            # total count 
            table[i][j] = x + y 
  
    return table[n][m-1] 


```

\begin{code}
-- makeChange :: [Int] -> Int -> (Int, [[Int]])
-- makeChange coins target = makeChange' [] coins (length coins) target
--   where
--     add :: (Int, [[Int]]) -> (Int, [[Int]]) -> (Int, [[Int]])
--     add (f,s) (f',s') = (f+f', s++s')

--     makeChange' :: [Int] -> [Int] -> Int -> Int -> (Int, [[Int]])
--     makeChange' coinSet coins coinIndex target
--       | target <  0 = (0, [])
--       | target == 0 = (1, [coinSet])
--       | coinIndex == 0 && target >= 1 = (0, [])
--       | otherwise = (makeChange' coinSet coins (coinIndex - 1) target) `add` (makeChange' (coinSet  ++ [coins !! (coinIndex - 1)]) coins coinIndex (target - (coins !! (coinIndex - 1))))

makeChange3 :: [Int] -> Int -> Int
makeChange3 coins target = makeChange' coins (length coins) target
  where
    makeChange' :: [Int] -> Int -> Int -> Int
    makeChange' coins coinIndex target
      | target <  0 = 0
      | target == 0 = 1
      | coinIndex == 0 && target >= 1 = 0
      | otherwise = (makeChange' coins (coinIndex - 1) target) + (makeChange' coins coinIndex (target - (coins !! (coinIndex - 1))))

-- makeChange :: [Int] -> Int -> Int -> Int
-- makeChange s m n =
--   let ij = 
--   map (\i ->
--     map (\j ->
      
--       ) [(s !! i)..(n+1)])
--     ) [0..m-1]

  
-- mkTable :: [Int] -> [(Int,Int)] -> Map.Map Int Int
-- mkTable s ijs = mkTable' s ijs Map.empty
--   where
--     mkTable' :: [Int] -> [(Int,Int)] -> Map.Map Int Int -> Map.Map Int Int
--     mkTable' s [] mm = mm
--     mkTable' s ((i,j): ijs) mm =
--       case Map.lookup (j - (s!!i)) mm of
--         Just oldVal -> mkTable' s ijs $ Map.update (\v -> Just (v + oldVal)) j mm
--         Nothing -> mkTable' s ijs (Map.insert j 0 mm)

{-
mkIndices :: [Int] -> Int -> [(Int, Int)]
mkIndices s n = [(i,j) | i <- [0..m-1], j <- [(s!!i)..(n)]]
  where m = length s

mkTable :: [Int] -> Int -> [(Int,Int)] -> Map.Map Int Int
mkTable s n ijs =
  let mmm = Map.update (\_ -> Just 1) 0 $ Map.fromList $ zip (take (n+1) [0..]) (replicate (n+1) 0) in
  foldl (\mm (i,j) ->
      case Map.lookup (j - (s!!i)) mm of
        Just oldVal -> Map.update (\v -> Just (v + oldVal)) j mm
        Nothing     -> Map.insert j 0 mm
  ) mmm ijs

makeChange2 :: [Int] -> Int -> (Maybe Int, Map.Map Int Int)
makeChange2 s n = (Map.lookup n table, table)
  where
    ijs = mkIndices s n
    table = mkTable s n ijs 

ttt :: Map.Map Int Int
ttt =
  let mmm = Map.fromList $ zip (take 4 [0..]) (replicate 4 2) in
  foldl (\mm i ->
      case Map.lookup i mm of
        Just oldVal -> Map.update (\v -> Just (v * i)) i mm
        Nothing     -> Map.insert i 0 mm
  ) mmm [1,2,3,4,5,6,7,8]
-}

\end{code}

```
def count(S, m, n): 
  
    # table[i] will be storing the number of solutions for 
    # value i. We need n+1 rows as the table is constructed 
    # in bottom up manner using the base case (n = 0) 
    # Initialize all table values as 0 
    table = [0 for k in range(n+1)] 
  
    # Base case (If given value is 0) 
    table[0] = 1
  
    # Pick all coins one by one and update the table[] values 
    # after the index greater than or equal to the value of the 
    # picked coin 
    for i in range(0,m): 
        for j in range(S[i],n+1):
            print (i, j)
            table[j] += table[j-S[i]] 
    print table  
    return table[n] 
```

```
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
```


\begin{code}

mkTable :: [Int] -> Int -> [(Int,Int)]
mkTable coinValues targetValue = [(i,j) | i <- [0..targetValue], j <- filter (\c -> c <= i) coinValues]

{-
dpMakeChange :: [Int] -> Int -> (Map.Map Int Int, Map.Map Int Int)
dpMakeChange coinValues targetValue = r
  where
    table = mkTable coinValues targetValue

    r = foldl (\(minCoins, coinsUsed) (centValue, j) ->
        let (coinCount, newCoin) =
              case Map.lookup (centValue - j) minCoins of
                Just newnew  -> if newnew + 1 < centValue then (newnew + 1, j) else (centValue, j)
                Nothing -> (centValue , 1)
        in (Map.update (\_ -> Just coinCount) centValue coinsUsed, Map.update (\_ -> Just newCoin) centValue coinsUsed)

        ) (Map.empty, Map.empty) table
-}
initMap :: Int -> Map.Map Int Int
initMap x = Map.fromList $ zip [0..x] (replicate (x+1) 0)

-- initMap :: Int -> Map.Map Int Int
-- initMap x = Map.fromList $ zip [0..x] (replicate x 0)


dpMakeChange :: [Int] -> Int -> [Int]
dpMakeChange coinValues change = filterUsedCoins coinsUsedMap change []
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

{-
dpMakeChange :: [Int] -> Int -> (Map.Map Int Int, Map.Map Int Int, Int, Int)
dpMakeChange coinValues change =
    foldl (\(minCoins, coinsUsed, _, _) cents ->
      -- let coinCount = cents
      --    newCoin   = 1 in
      foldl (\(minCoins, coinsUsed, coinCount, newCoin) j ->
        let (coinCount', newCoin') =
             case Map.lookup (cents - j) minCoins of
               Just res -> 
                 if (res + 1 < coinCount)
                   then (res + 1, j)
                   else (coinCount, newCoin)
               Nothing -> (coinCount, newCoin)
        in (Map.update (\_ -> Just coinCount') cents minCoins, Map.update (\_ -> Just newCoin') cents coinsUsed, coinCount', newCoin')
      ) (minCoins, coinsUsed, cents, 1) (filter (\c -> c <= cents) coinValues)
    ) (initMap change, initMap change, 0, 0) [0..change]
-}

-- dpMakeChange :: [Int] -> Int -> Int -> [Int]
-- dpMakeChange coinValueList change minCoins coinUsed =
--   map (\cents ->
--     let coinCount = cents
--     let newCoin = 1
--     let js = filter (\c -> c <= cents) coinValueList

--     map (\j ->
--       if (minCoins !! (cents - j)) + 1 < coinCount
--         then coinCount = (minCoins!!(cents-j))+1
--              newCoin = j
--         else ()
--       minCoins !! cents = coinCount
--       coinsUsed !! cents = newCoin
--     ) js
--   ) [0..change]
-- ll = [(i,j) | i <- [1..3] , j <- [i..4]]

-- ll :: [(Int, Int)]
-- ll = do
--    i <- [1..3]
--    j <- [i..4]
--    pure (i,j)

test = [1,2,3]

main :: IO ()
main = do
  print $ minimumChangeSolution test 4
  print $ dpMakeChange test 4
--   print $ makeChange2 test 4
--  print $ mkIndices test 4

\end{code}
