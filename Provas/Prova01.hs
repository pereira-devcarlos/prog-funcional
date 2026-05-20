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

---- Questão 2
{-
Considerando f_in, faça, em Haskell, uma solução que receba um Int x e uma [[Int]] l.
Como saída, teremos uma [(Bool, [Int])] s que informa, para cada sublista li de l, se o elemento
x pertence ou não à li.
-}
f2 :: Int -> [[Int]] -> [(Bool, [Int])]
f2 _ [] = []
f2 x (y:ys) = (f_in x y, y) : f2 x ys

---- Questão 3
{-
Considerando f2, faça, em Haskell, uma solução que receba um Int x e uma [[Int]] l.
Como saída, teremos uma (Int, [(Bool, [Int])]) s que informa o Int x e, para cada sublista li
de l, se o elemento x pertence ou não à li.
-}
f3 :: Int -> [[Int]] -> (Int, [(Bool,[Int])])
f3 x y = (x, f2 x y)
