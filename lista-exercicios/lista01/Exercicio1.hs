--------------------------------------
---Exercicio.hs
--------------------------------------

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

main :: IO()
main = do 
    putStrLn "Função 1"
    print(f1 2)
    print(f1 (-2))
    putStrLn ""

    putStrLn "Função 2"
    print(f2 (4, 2))
    print(f2 (-3, 3))
    putStrLn ""

    putStrLn "Função 3"
    print(f3 (2, 2, 3))
    print(f3 (1, 2, 5))
    print(f3 (2, 2, 4))