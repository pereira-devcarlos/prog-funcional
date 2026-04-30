{- Assunto: listas e tuplas -}

periodo::Int
periodo = 7

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise = n

-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

{- 01 função que retorna uma lista de vendas -}
listaVendas :: Int-> [Int]
listaVendas x 
    | x > periodo = []
    | otherwise = vendas x : listaVendas (x+1)


{- 02 função que retorna [[Int]] com listas de dia e venda -}
f2L::Int->[[Int]]
f2L 0 = []
f2L x = [x,vendas x]:f2L (x-1)  


----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
ordenaLista::[Int] -> [Int]
ordenaLista x = sortList x (length x)

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


-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
ordenaListaLista::[[Int]]->[[Int]]
ordenaListaLista x = sortListList x (length x)

sortPartListList :: [[Int]] -> [[Int]]
sortPartListList [] = []
sortPartListList [a] = [a]
sortPartListList (x:y:ys)
    | head x > head y = y : sortPartListList(x : ys)
    | otherwise = x : sortPartListList(y : ys)

sortListList :: [[Int]] -> Int -> [[Int]]
sortListList [] _ = []
sortListList l 1 = l
sortListList l n = sortListList (sortPartListList l) (n-1)


---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
ordenaLILE::[[Int]] -> [[Int]]
ordenaLILE [] = []
ordenaLILE x = ordenaListaLista (ordenaLILEAux x)

ordenaLILEAux :: [[Int]] -> [[Int]]
ordenaLILEAux [] = []
ordenaLILEAux (x:xs) = ordenaLista x : ordenaLILEAux xs


-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
f6T :: Int-> [(Int, Int)]
f6T 0 = []
f6T x = (x,vendas x):f6T (x-1)

{- 07 função que gera o total de vendas-}
totalVendasT::[(Int, Int)] -> Int
totalVendasT [] = 0
totalVendasT (x:xs) = snd (x) + totalVendasT xs

{- 08 função que retorna a maior venda -}
 
--maiorVendasT8a::Int-> [(Int, Int)] -> Int 
  
{- 08-b como implementar com apenas os parâmetros? -}
maiorVendaT8b::[(Int, Int)] -> Int    
maiorVendaT8b [] = 0
maiorVendaT8b ((a,b):c) = maxi b (maiorVendaT8b c)

maiorVendaT8c::[(Int, Int)] -> Int    
maiorVendaT8c [] = 0
maiorVendaT8c (a:c) = maxi (snd a) (maiorVendaT8c c)

maiorVendaT8d::[(Int, Int)] -> Int    
maiorVendaT8d [] = 0
maiorVendaT8d c = maxi (snd(head c)) (maiorVendaT8d (tail c))
  
{- 09 função que retorna os dias das maiores vendas -}