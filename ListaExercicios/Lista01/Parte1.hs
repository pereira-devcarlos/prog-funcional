--------------------------------------
---Exercicio 1
--------------------------------------
-- Funções com condicionais
f1 :: Double -> Double
f1 x
    | x >=0 = x+4/x+2
    | x < 0 = 2/x

f2 :: (Double, Double) -> Double
f2 (x,y)
    | x >= y = x + y
    | x < y = x - y

f3 :: (Double, Double, Double) -> Double
f3 (x,y,z)
    | (x+y) > z = x + y + z
    | (x+y) < z = x - y - z
    | (x+y) == z = 0

--------------------------------------
---Exercicio 2
--------------------------------------
-- Fatorial de um número usando recursão
fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x-1)

--------------------------------------
---Exercicio 3
--------------------------------------
-- Multiplicação de dois números usando apenas soma
soma :: Int -> Int -> Int
soma a b = a + b

mult :: Int -> Int -> Int
mult a b
    | a == 0 = 0
    | otherwise = soma b (mult b (a-1))

--------------------------------------
---Exercicio 4
--------------------------------------
-- Inverter um número inteiro 
invertInt :: Int -> Int
invertInt x = aux x 0

aux :: Int -> Int -> Int
aux 0 tmp2 = tmp2
aux tmp1 tmp2 = aux (tmp1`div`10) (tmp2 * 10 + tmp1 `mod` 10)

--------------------------------------
---Exercicio 5
--------------------------------------
-- Função que calcula a quarta potência de um número
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square x * square x