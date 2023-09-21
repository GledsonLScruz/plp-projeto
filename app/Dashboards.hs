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

-- Função para calcular a quantidade total de produtos em estoque
quantidadeTotalEstoque :: [Produto] -> Int
quantidadeTotalEstoque produtos = sum (map quantidade produtos)

-- Função para listar produtos com estoque baixo (estoque < 10)
produtosComEstoqueBaixo :: [Produto] -> [Produto]
produtosComEstoqueBaixo produtos = filter (\p -> getQuantidade p < 10) produtos

-- Função para calcular a receita total gerada por todos os produtos
receitaTotalPorProduto :: [Produto] -> Double
receitaTotalPorProduto produtos = sum $ map (\p -> getPrecoVenda p * fromIntegral (getQuantidade p)) produtos

-- Função para calcular a receita total gerada por categoria
receitaTotalPorCategoria :: [Produto] -> String -> Double
receitaTotalPorCategoria produtos categoria = 
  sum $ map (\p -> if getCategoria p == categoria then getPrecoVenda p * fromIntegral (getQuantidade p) else 0) produtos

-- Função para encontrar os produtos mais populares
produtosMaisPopulares :: [Produto] -> [Produto]
produtosMaisPopulares produtos =
  let produtosOrdenados = reverse (sortOn quantidadeVendida produtos)
  in take 5 produtosOrdenados
  where
    quantidadeVendida produto = quantidade produto - quantidadeEstoque produto
    quantidadeEstoque produto = max 0 (quantidade produto)