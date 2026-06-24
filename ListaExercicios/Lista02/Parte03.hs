---Parte 3 - Lista 02
--Exercícios 11 à 15
--------------------------------------

--------------------------------------
---Exercício 12
--------------------------------------
evenCubes :: Int -> [Int]
evenCubes x = map (^3) (filter even [1..(x-1)])

--------------------------------------
---Exercício 13
--------------------------------------
insertOrd :: Int -> [Int] -> [Int]
insertOrd x xs = [a | a<- xs, a < x] ++ [x] ++ [b | b<-xs, b>=x]

--------------------------------------
---Exercício 14
--------------------------------------
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x a b = sum [1 | y<-[a..b], y`mod`x == 0]

--------------------------------------
---Exercício 15
--------------------------------------
duplicate :: String -> Int -> String
duplicate x y = concat [x | _<-[1..y]]