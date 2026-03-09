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

main :: IO()
main = do 
    print(f1 2)
    print(f1 (-2))
    
    print(f2 (4, 2))
    print(f2 (-3, 3))