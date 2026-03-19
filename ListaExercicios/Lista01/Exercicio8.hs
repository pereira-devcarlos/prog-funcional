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