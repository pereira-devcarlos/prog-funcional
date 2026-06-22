---Parte 1 - Lista 02
--Exercícios 01 à 05
--------------------------------------

--------------------------------------
---Exercício 1
--------------------------------------
--- Utilizando List comprehension
-- Crie uma função que calcule 1^2 + 2^2 + ... + 100^2
sumSquare :: Int
sumSquare = sum [a^2 | a <- [1..100]]

--------------------------------------
---Exercício 2
--------------------------------------
replicate01 :: Int -> a -> [a]
replicate01 x y = [y | _ <-[1..x]]

--------------------------------------
---Exercício 3
--------------------------------------
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x, y, z) | x <-[1..a], y<-[1..a], z<-[1..a], x^2 + y^2 == z^2]

--------------------------------------
---Exercício 4
--------------------------------------
sumDivisors :: Int -> Int
sumDivisors x = sum [a | a <-[1..(x-1)], x `mod` a == 0]

perfects :: Int -> [Int]
perfects n = [b | b<-[1..n], (sumDivisors b) == b]