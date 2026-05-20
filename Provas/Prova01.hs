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

---- Quetão 4
{-
Faça, em Haskell, uma solução que receba uma (Int,[(Bool,[Int])]) e gere uma [[Int]]
com apenas as listas [Int] cujo Bool da dupla é False.
-}
f4 :: (Int, [(Bool,[Int])]) -> [[Int]]
f4 (_, []) = []
f4 (x, (y:ys))
    | fst y = f4 (x, ys)
    | otherwise = snd y : f4 (x, ys)

---- Questão 5
{-
Faça, em Haskell, uma solução que receba duas strings S e R, cuja menor tem tamanho
n. Sua solução deverá retornar uma lista de Bool B de tamanho n, de modo que cada Bi será
a informação se Si é igual ou diferente de Ri, para 0 ≤ i < n.
-}
f5 :: String -> String -> [Bool]
f5 [] _ = []
f5 _ [] = []
f5 (x:xs) (y:ys) = (x == y) : (f5 xs ys)