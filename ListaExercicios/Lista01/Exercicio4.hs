--------------------------------------
---Exercicio4.hs
--------------------------------------

invertInt :: Int -> Int
invertInt x = aux x 0

aux :: Int -> Int -> Int
aux 0 tmp2 = tmp2
aux tmp1 tmp2 = aux (tmp1`div`10) (tmp2 * 10 + tmp1 `mod` 10)

main = do
    print(invertInt 12345)