--------------------------------------
---Prova01.hs
--------------------------------------
---- Questão 1
{-
Faça, em Haskell, uma solução que receba um Int a e uma [Int] b. Como saída,
teremos um Bool que informa se a ∈ b.
-}
f_in :: Int -> [Int] -> Bool
f_in _ [] = False
f_in x (y:ys) = x == y || f_in x ys