--------------------------------------
---Exercicio5.hs
--------------------------------------

square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square x * square x

main :: IO()
main = do
    print(square 5)
    print(fourPower 5)