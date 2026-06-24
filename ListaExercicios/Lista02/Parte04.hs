---Parte 4 - Lista 02
--Exercícios 16 à 21
--------------------------------------
import Data.Char

--------------------------------------
---Exercício 16
--------------------------------------
{- HLINT ignore "Avoid lambda" -}
pushRight :: String -> Int -> String
pushRight x y = ['>' | a<-[1..(y- length x)]] ++ x

--------------------------------------
---Exercício 17
--------------------------------------
inverte :: [Int] -> [Int]
inverte = foldl (\a x -> x:a) []

--------------------------------------
---Exercício 18
--------------------------------------
separa :: [Int] -> ([Int],[Int])
separa x = ([a | a<-x, odd a],[b | b<-x, even b])

--------------------------------------
---Exercício 19
--------------------------------------
converte :: [Int] -> String
converte x = [chr (ord 'A' + (a-1)) | a<-x]

--------------------------------------
---Exercício 20
--------------------------------------
conta :: String -> Char -> Int
conta xs x = sum [1 | c<-xs, c == x]

--------------------------------------
---Exercício 20.a)
--------------------------------------
proliferaInt :: [Int] -> [Int]
proliferaInt x = concat [[y | _<-[1..y]] | y<-x]

--------------------------------------
---Exercício 21
--------------------------------------
proliferaChar :: String -> String
proliferaChar x = concat [[y | _<-[1..(ord y - ord 'A' + 1)]] | y<-x]