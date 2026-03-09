--------------------------------------
---Exercicio3.hs
--------------------------------------

soma :: Int -> Int -> Int
soma a b = a + b

mult :: Int -> Int -> Int
mult a b
    | a == 0 = 0
    | otherwise = soma b (mult b (a-1))

multiplicacao :: Int -> Int -> Int
multiplicacao 0 b = 0
multiplicacao a b = soma b (multiplicacao b (a-1))

main :: IO()
main = do
    print(soma 5 10)
    print(mult 3 5)
    print(multiplicacao 4 5)