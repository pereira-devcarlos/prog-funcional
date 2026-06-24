---------------------------------------------
---Aula03.hs
---------------------------------------------
{- HLINT ignore "Use concat" -}
{- HLINT ignore "Use even" -}

-- Exercise 1
square :: [Int] -> [Int]
square = map (^2)

-- Exercise 2
multiples5 :: [Int] -> [Int]
multiples5 =  filter xs
    where xs a = a `mod` 5 == 0

-- Exercise 3
squareOfEven :: [Int] -> [Int]
squareOfEven x = map (^2) (filter xs x)
    where xs a = a `mod` 2 == 0

-- Exercise 4
concatenate :: [[Int]] -> [Int]
concatenate = foldr (++) [] 

-- Exercise 5
myReverse :: [[Int]] -> [Int]
myReverse = foldl (++) []