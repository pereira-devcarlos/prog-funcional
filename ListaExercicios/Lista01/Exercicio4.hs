--------------------------------------
---Exercicio4.hs
--------------------------------------

divisao :: Int -> Int
divisao x = x `div` 10

modulo :: Int -> Int
modulo x = x `mod` 10

main = do
    print(divisao 123)
    print(modulo 123)