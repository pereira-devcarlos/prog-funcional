--------------------------------------
---Exercicio 11
--------------------------------------
-- Encontre o dígito de um número em uma posição específica
tamanhoInt :: Int -> Int
tamanhoInt num = tamanho num 0

tamanho :: Int -> Int -> Int
tamanho 0 a = a
tamanho num a = tamanho (num `div` 10) (a + 1)

anyDigit :: Int -> Int -> Int
anyDigit i num = anyDigitAux i num (tamanhoInt num)

anyDigitAux :: Int -> Int -> Int -> Int
anyDigitAux i num tam
    | tam <= i || i < 0 = -1
    | tam == (i+1) = num `mod` 10
    | otherwise = anyDigitAux i (num `div` 10) (tam - 1)

----------------------------------------
---Exercicio 12
----------------------------------------
-- Verifique se três números são diferentes entre si
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

----------------------------------------
---Exercicio 13
----------------------------------------
-- Conte quantos números iguais existem entre três números
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) = 2
    | otherwise = 0