---Parte 2 - Lista 02
--Exercícios 06 à 10
--------------------------------------

--------------------------------------
---Exercício 6
--------------------------------------
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = length xs - 1

find :: Eq a => a -> [(a, b)] -> [b]
find x y = [b | (a,b) <-y, a==x]

--------------------------------------
---Exercício 7
--------------------------------------
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct x y = sum [a*b | (a,b)<- zip x y]

--------------------------------------
---Exercício 8
--------------------------------------
(&!) :: Int -> Int -> Int
x &! 0 = 1
x &! n = x * (x &! (n-1))

--------------------------------------
---Exercício 9
--------------------------------------
sumOdd :: (Int -> Int) -> [Int] -> [Int]
sumOdd f x = map f (filter odd x)

--------------------------------------
---Exercício 10
--------------------------------------
dec2Int :: [Int] -> Int
dec2Int x = sum [a * 10&!b | (a,b)<- zip x (reverse [0..n])]
    where n = length x - 1