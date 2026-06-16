---------------------------------------------
---Aula03.hs
---------------------------------------------

-- Exercise 1
square :: [Int] -> [Int]
square x = map (^2) x

-- Exercise 2
multiples5 :: [Int] -> [Int]
multiples5 x =  filter xs x
    where xs a = a `mod` 5 == 0

-- Exercise 3
squareOfEven :: [Int] -> [Int]
squareOfEven x = map (^2) (filter xs x)
    where xs a = a `mod` 2 == 0