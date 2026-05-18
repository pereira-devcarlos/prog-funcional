{- objetivo: trabalhar tipos distintos entre listas e tuplas -}

import Data.Char

------------------------------------------------------
{- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}
myUnzip :: [(Int,Char)] -> ([Int],[Char])
myUnzip [] = ([],[])
myUnzip ((x,y):ys) = (x: fst(myUnzip ys), y: snd(myUnzip ys))

{- 02 versão em uma única função -}
--myUnzipU :: [(Int,Char)]->([Int],[Char])

------------------------------------------------------------
{- 03 função que junta duas listas em lista de duplas -}
myZip::[Bool]->[Char] ->[(Bool,Char)]
myZip [] [] = []
myZip (x:xs) (y:ys) = (x,y): myZip xs ys   

{- 04 função que recebe [Char] e retorna [(Bool,Char)] 
   True se Char for alfanumérico e False, caso contrário -}
setAlfa::String -> [(Bool,Char)]
setAlfa [] = []
setAlfa (x:xs) = [(ord x >= 48 && ord x <= 90 || ord x >= 97 && ord x <= 122, x)] ++ setAlfa xs


{- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
--filtraAlfa:: [(Bool,Char)] -> String

{- 06 função transforma String de alfa em Int -}
--alfaToInt::String -> [Int]


{-- 07 função que gera tabela ascii -}
--geraASCII::Int->[(Int,Char)]