--------------------------------------
---Exercicio.hs
--------------------------------------

a :: Double -> Double
a x
    | x >=0 = x+4/x+2
    | x < 0 = 2/x

main :: IO()
main = do 
    print(a 2)
    print(a (-2))