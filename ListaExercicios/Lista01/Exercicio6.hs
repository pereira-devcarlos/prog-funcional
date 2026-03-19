--------------------------------------
---Exercicio 6
--------------------------------------
-- Calcular a raiz quadrada de 6 n vezes
raiz :: Int -> Double 
raiz 0 = 0
raiz x = sqrt (6 + raiz (x - 1))

--------------------------------------
---Exercicio 7
--------------------------------------
-- Quantas maneiras diferentes existem 
-- para escolher n elementos de um 
-- conjunto de m elementos?
fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x-1)

maneiras :: Int -> Int -> Int
maneiras n m 
    | n > m = 0
    | otherwise = fat m `div` (fat n * fat(m-n))