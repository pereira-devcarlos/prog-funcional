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

-- Função para listar as vendas de cada dia do período
listSales :: [Int]
listSales = listSalesAux 1

listSalesAux :: Int -> [Int]
listSalesAux x
    | x > periodo = []
    | otherwise = vendas x : listSalesAux(x+1)

-- Função para listar o dia e a venda correspondente
dayAndSale :: Int -> [Int]
dayAndSale x = x : vendas x : []

-- Função para listar o dia e a venda correspondente de cada dia do período
listDaysAndSales :: [[Int]]
listDaysAndSales = listDaysAndSalesAux 1

listDaysAndSalesAux :: Int -> [[Int]]
listDaysAndSalesAux x
    | x > periodo = []
    | otherwise = dayAndSale x : listDaysAndSalesAux(x+1)

-- Função para ordenar uma parte da lista
sortPartList :: [Int] -> [Int]
sortPartList [] = []
sortPartList [a] = [a]
sortPartList (x:y:tail)
    | x > y = y : sortPartList (x : tail)
    | otherwise = x : sortPartList (y : tail)


-- Função para ir ordenando a lista até que ela esteja completamente ordenada
sortList :: [Int] -> Int -> [Int]
sortList [] _ = []
sortList l 1 = l 
sortList l n = sortList (sortPartList l) (n-1)

-- Função para ordenar uma lista inspirado em bubble sort
bubbleSort :: [Int] -> [Int]
bubbleSort l = sortList l (length l)