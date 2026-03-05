--------------------------------------
---Fatorial.hs
--------------------------------------

fatorial :: Int -> Int
fatorial x
    -- Condição de parada
    | x == 0 = 1 
    | otherwise = x * fatorial(x - 1)

main :: IO()
main = do
    print(fatorial 3)
    print(fatorial 4)
    print(fatorial 5)