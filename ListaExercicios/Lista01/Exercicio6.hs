--------------------------------------
---Exercicio6.hs
--------------------------------------

raiz :: Int -> Double 
raiz 0 = 0
raiz x = sqrt (6 + raiz (x - 1))

main = do
    print(sqrt 6)
    print(raiz 1)
    print(raiz 30)