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