--------------------------------------
---Exercicio4.hs
--------------------------------------

invertInt :: Int -> Int
invertInt x = aux x 0

aux :: Int -> Int -> Int
aux 0 acumulador = acumulador
aux resto acumulador = aux (resto`div`10) (acumulador * 10 + resto `mod` 10)

main = do
    print(invertInt 12345)