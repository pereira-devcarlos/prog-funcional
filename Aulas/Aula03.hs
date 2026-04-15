import System.Win32 (xBUTTON1)
---------------------------------------------
---Aula03.hs
---------------------------------------------

-- Função para somar os elementos de uma lista
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Função para encontrar o maior elemento de uma lista
double :: [Int] -> [Int]
double []=[]
double (x:xs) = (x*2) : double xs

-- Função para verificar se o elemento pertence a lista
member :: Int -> [Int] -> Bool
member _ [] = False
member x (y:ys)
    | x == y = True
    | otherwise = member x ys

periodo :: Int
periodo = 7

maxi :: Int -> Int -> Int
maxi m n
    | m >= n = m
    | otherwise = n

vendas :: Int -> Int
vendas 1 = 24
vendas 2 = 15
vendas 3 = 17
vendas 4 = 20
vendas 5 = 22
vendas 6 = 18
vendas 7 = 25
vendas _ = 0

listSales :: [Int]
listSales = listSalesAux 1

listSalesAux :: Int -> [Int]
listSalesAux x
    | x > periodo = []
    | otherwise = vendas x : listSalesAux(x+1)

dayAndSale :: Int -> [Int]
dayAndSale x = x : vendas x : []

listDaysAndSales :: [[Int]]
listDaysAndSales = listDaysAndSalesAux 1

listDaysAndSalesAux :: Int -> [[Int]]
listDaysAndSalesAux x
    | x > periodo = []
    | otherwise = dayAndSale x : listDaysAndSalesAux(x+1)