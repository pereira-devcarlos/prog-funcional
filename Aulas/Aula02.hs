-------------------------------------
---Aula02.hs
-------------------------------------
-- Vendas de uma loja em um período de 8 dias

-- Número de dias no período de vendas
periodo :: Int
periodo = 8  

-- Função para obter as vendas de um determinado dia
vendas :: Int -> Int
vendas 1 = 24
vendas 2 = 15
vendas 3 = 17
vendas 4 = 20
vendas 5 = 22
vendas 6 = 18
vendas 7 = 25
vendas 8 = 1
vendas _ = 0

-- Função para calcular o total de vendas de forma crescente
totalVendasCrescente :: Int -> Int
totalVendasCrescente x 
    | x == periodo = vendas periodo
    | otherwise = vendas x + totalVendasCrescente(x + 1)

-- Função para calcular o total de vendas de forma decrescente
totalVendasDecrescente :: Int -> Int
totalVendasDecrescente x
    | x == 0 = vendas 0
    | otherwise = vendas x + totalVendasDecrescente(x - 1)



main :: IO()
main = do
    print(vendas 3)
    print(totalVendasCrescente 0)
    print(totalVendasDecrescente periodo)