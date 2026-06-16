---------------------------------------------
---Aula03.hs
---------------------------------------------

-- Exercice 1
square :: [Int] -> [Int]
square x = map (^2) x

-- Exercice 2
multiples5 :: [Int] -> [Int]
multiples5 x =  filter xs x
    where xs a = a `mod` 5 == 0