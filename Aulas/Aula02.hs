---------------------------------------------
---Aula02.hs
---------------------------------------------
-- Vendas de uma loja em um período de 8 dias

---------------------------------------------
-- Definindo os tipos
type Dia = Int
type Vendas = Int
type Periodo = Int

---------------------------------------------
-- Número de dias no período de vendas
periodo :: Periodo
periodo = 8  

---------------------------------------------
-- Função para obter as vendas de um dia
vendas :: Dia -> Vendas
vendas 1 = 24
vendas 2 = 15
vendas 3 = 17
vendas 4 = 20
vendas 5 = 22
vendas 6 = 18
vendas 7 = 25
vendas 8 = 1
vendas _ = 0

---------------------------------------------
-- Função para calcular o total de vendas
-- Forma crescente
totalVendasCrescente :: Periodo -> Vendas
totalVendasCrescente x 
    | x == periodo = vendas periodo
    | otherwise = vendas x + totalVendasCrescente(x + 1)

----------------------------------------------
-- Função para calcular o total de vendas
-- Forma decrescente
totalVendasDecrescente :: Periodo -> Vendas
totalVendasDecrescente x
    | x == 0 = vendas 0
    | otherwise = vendas x + totalVendasDecrescente(x - 1)

----------------------------------------------
-- Função maior
-- Encontra o maior valor entre dois números
maior :: Int -> Int -> Int
maior x y
    | x > y = x
    | otherwise = y

----------------------------------------------
-- Função para encontrar a maior venda no período
maiorVendas :: Periodo -> Vendas
maiorVendas x
    | x == 0 = vendas 0
    | otherwise = maior (vendas x) (maiorVendas(x-1))

----------------------------------------------
-- Função para encontrar o dia com a maior venda no período
-- Parâmetro: período (número de dias)
diaMaiorVendas01 :: Periodo -> Dia
diaMaiorVendas01 x
    | x == 0 = 0
    | vendas x > vendas(diaMaiorVendas01(x-1)) = x
    | otherwise = diaMaiorVendas01(x-1)

----------------------------------------------
-- Função alternativa para encontrar o dia com a maior venda no período
-- Parâmetros: período e a maior venda
diaMaiorVendas02 :: Periodo -> Vendas -> Dia
diaMaiorVendas02 0 _ = 0
diaMaiorVendas02 p v
    | vendas p == v = p
    | otherwise = diaMaiorVendas02 (p - 1) v

----------------------------------------------
-- Função para calcular a média de vendas por dia
mediaVendas :: Periodo -> Vendas -> Float
mediaVendas x total = fromIntegral total / fromIntegral x

----------------------------------------------
-- Função para contar quantos dias têm vendas menores que um valor
-- em um determinado intervalo de dias
howManyLess :: Int -> Int -> Int -> Int
howManyLess valor inicio fim = numDias valor inicio fim 0


numDias :: Int -> Int -> Int -> Int -> Int
numDias valor inicio fim dias
    | inicio > fim || inicio > periodo = dias
    | vendas inicio < valor = numDias valor (inicio + 1) fim (dias + 1)
    | otherwise = numDias valor (inicio + 1) fim dias

----------------------------------------------
-- Função para verificar se não houve vendas zero em um período
noZeroInPeriod :: Int -> Bool
noZeroInPeriod 0 = True
noZeroInPeriod x
    | vendas x == 0 = False
    | otherwise = noZeroInPeriod (x - 1)