--------------------------------------
---Exercicio7.hs
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