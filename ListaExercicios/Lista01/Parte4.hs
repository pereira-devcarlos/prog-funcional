import System.Win32 (COORD(yPos), xBUTTON1)
import Data.Char
import Graphics.Win32 (rgb)
---Parte 3 - Lista 01
--Exercícios 16 à 20
--------------------------------------

--------------------------------------
---Exercício 16
--------------------------------------
funny :: Ord a => a -> a -> a -> Bool
funny x y z = (x>z) && (y<x)

--------------------------------------
---Exercício 22
--------------------------------------
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x] 

--------------------------------------
---Exercício 18
--------------------------------------
separa :: [Int] -> ([Int],[Int])
separa [] = ([],[])
separa (x:xs)
    | (x `mod` 2) /= 0 = (x: fst (separa xs), snd (separa xs))
    | otherwise = (fst (separa xs), x: snd (separa xs))

--------------------------------------
---Exercício 26
--------------------------------------
conta :: String -> Char -> Int
conta [] _ = 0
conta (x:xs) y
    | x == y = 1 + conta xs y
    | otherwise = conta xs y

--------------------------------------
---Exercício 27
--------------------------------------
purifica :: [Int] -> [Int]
purifica [] = []
purifica [a] = [a]
purifica (x:y:ys)
    | x == y = purifica (y:ys)
    | otherwise = x : purifica (y:ys)

----------------------------------------
---Exercício 28
----------------------------------------
proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (x:xs) = prolifera x 0 ++ proliferaInt xs

prolifera :: Int -> Int -> [Int]
prolifera 0 _ = []
prolifera x y = x+y: prolifera (x-1) (y+1) 

-----------------------------------------
---Exercício 30
-----------------------------------------
converte :: Char -> (Char, Char, Int)
converte x = (x, chr(ord x - 32), ord x)

-- Funcões auxiliares para o exercício 33
myF :: (a, b, c) -> a
myF (a,_,_) = a

myS :: (a, b, c) -> b
myS (_,b,_) = b

myT :: (a, b, c) -> c
myT(_,_,c) = c

------------------------------------------
---Exercício 33
-------------------------------------------
--A função recebe duas datas, cada uma representada por uma tupla (dia, mês, ano), e retorna a diferença em dias entre elas.
difDatas :: (Int, Int, Int) -> (Int, Int, Int) -> Int
difDatas x y 
    | myT x == myT y && myS x == myS y = difDias (myF x) (myF y)
    | myT x == myT y = difMes (myF x, myS x) (myF y, myS y)
    | otherwise = (12 - myS x) * 31 - myF x + (myF y + 31 * (myS y - 1))

-- Função auxiliar para retornar a diferença em dias entre dois dias do mesmo mês e ano
difDias :: Int -> Int -> Int
difDias x y = y - x 

-- Função auxiliar para retornar a diferença em dias entre dois meses do mesmo ano
difMes :: (Int, Int) -> (Int, Int) -> Int
difMes x y
    | snd x == snd y = fst y - fst x
    | otherwise = (31 - fst x) + fst y + 31 * ((snd y - snd x) - 1)