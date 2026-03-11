--------------------------------------
---Exercicio4.hs
--------------------------------------

div10 :: Int -> Int
div10 x = x `div` 10

mod10 :: Int -> Int
mod10 x = x `mod` 10

modMulti :: Int -> Int -> Int
modMulti x y = x * 10 + y

invertInt :: Int -> Int
invertInt x = aux x 0

aux :: Int -> Int -> Int
aux 0 acumulador = acumulador
aux resto acumulador = aux (div10 resto) (acumulador * 10 + mod10 resto)

main = do
    print(div10 1)
    print(mod10 1)
    print(invertInt 123)