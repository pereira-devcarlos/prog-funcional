--------------------------------------
---Exercicio7.hs
--------------------------------------

fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x-1)

maneiras :: Int -> Int -> Int
maneiras n m 
    | n > m = 0
    | otherwise = fat m `div` (fat n * fat(m-n))

main :: IO()
main = do
    print(fat 5)
    print(maneiras 1 3)
    print(maneiras 3 20)