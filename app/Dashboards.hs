module Dashboards where

import Cliente
import Produto
import ProdutoRepository
import Data.List (sortOn)
{-
-- Função para calcular os clientes mais ativos
clientesMaisAtivos :: [Cliente] -> [Cliente]
clientesMaisAtivos clientes = take 5 $ reverse $ sortOn (\cliente -> length (historicoCompras cliente)) clientes

-- Função para calcular a média de compras por cliente
mediaComprasPorCliente :: [Cliente] -> Double
mediaComprasPorCliente clientes =
  let totalClientes = length clientes
      totalCompras = sum (map (length . historicoCompras) clientes)
   in fromIntegral totalCompras / fromIntegral totalClientes
-}
-- Função para exibir o dashboard de quantidade de produtos em estoque
exibirQuantidadeProdutosEstoque :: [Produto] -> IO ()
exibirQuantidadeProdutosEstoque produtos = do
  let totalEstoque = quantidadeTotalEstoque produtos
  putStrLn $ "Quantidade total de produtos em estoque: " ++ show totalEstoque

-- Função para calcular a quantidade total de produtos em estoque
quantidadeTotalEstoque :: [Produto] -> Int
quantidadeTotalEstoque produtos = sum (map quantidade produtos)