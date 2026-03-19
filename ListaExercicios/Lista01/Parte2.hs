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

--------------------------------------
---Exercicio 8 
--------------------------------------
-- Máximo divisor comum (MDC) entre dois números
mdc :: Int -> Int -> Int
mdc m n
    | m`mod`n == 0 = n
    | otherwise = mdc n (m `mod` n)

--------------------------------------
---Exercicio 9
--------------------------------------
-- Quantos múltiplos de um número existem em um intervalo
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples num inicio fim
    | inicio > fim = 0
    | inicio `mod` num == 0 = 1 + howManyMultiples num (inicio+1) fim
    | otherwise = howManyMultiples num (inicio+1) fim

--------------------------------------
---Exercicio 10
--------------------------------------
-- Último dígito de um número
lastDigit :: Int -> Int
lastDigit x = x `mod` 10