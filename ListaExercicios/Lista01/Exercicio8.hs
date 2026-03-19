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