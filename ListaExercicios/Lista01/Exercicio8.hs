--------------------------------------
---Exercicio 8 
--------------------------------------

mdc :: Int -> Int -> Int
mdc m n
    | m`mod`n == 0 = n
    | otherwise = mdc n (m `mod` n)

--------------------------------------
---Exercicio 9
--------------------------------------

howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples num inicio fim
    | inicio > fim = 0
    | inicio `mod` num == 0 = 1 + howManyMultiples num (inicio+1) fim
    | otherwise = howManyMultiples num (inicio+1) fim