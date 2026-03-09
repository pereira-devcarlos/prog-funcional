--------------------------------------
---Exercicio2.hs
--------------------------------------

fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x-1)

main :: IO()
main = do
    print(fat 5)