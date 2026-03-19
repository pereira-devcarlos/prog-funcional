---Parte 3 - Lista 01
--Exercícios 11 à 15
--------------------------------------

--------------------------------------
---Exercício 11
--------------------------------------
-- Encontre o dígito de um número em uma posição específica
tamanhoInt :: Int -> Int
tamanhoInt 0 = 0
tamanhoInt num = 1 + tamanhoInt (num `div` 10)

anyDigit :: Int -> Int -> Int
anyDigit i num = anyDigitAux i num (tamanhoInt num)

anyDigitAux :: Int -> Int -> Int -> Int
anyDigitAux i num tam
    | tam <= i || i < 0 = -1
    | tam == (i+1) = num `mod` 10
    | otherwise = anyDigitAux i (num `div` 10) (tam - 1)

----------------------------------------
---Exercício 12
----------------------------------------
-- Verifique se três números são diferentes entre si
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

----------------------------------------
---Exercício 13
----------------------------------------
-- Conte quantos números iguais existem entre três números
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) = 2
    | otherwise = 0

----------------------------------------
---Exercício 14
----------------------------------------
-- A resolução do exercício está em: 
-- Aulas/Aula02.hs
-- Letra a)
{-Função: howManyLess}

-- Letra b>
{-Função: noZeroInPeriod}

----------------------------------------
---Exercício 15
----------------------------------------

