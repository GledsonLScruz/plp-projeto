module Dashboards where

import Cliente
import Data.List (sortOn)

-- Função para calcular os clientes mais ativos
clientesMaisAtivos :: [Cliente] -> [Cliente]
clientesMaisAtivos clientes = take 5 $ reverse $ sortOn (\cliente -> length (historicoCompras cliente)) clientes

-- Função para calcular a média de compras por cliente
mediaComprasPorCliente :: [Cliente] -> Double
mediaComprasPorCliente clientes =
  let totalClientes = length clientes
      totalCompras = sum (map (length . historicoCompras) clientes)
   in fromIntegral totalCompras / fromIntegral totalClientes