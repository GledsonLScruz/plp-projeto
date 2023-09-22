module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv

import Controllers
import Auxiliares
import Tabela

main :: IO ()
main = do
  produtos <- lerProdutosCSV "produtos.csv"
  clientes <- lerClientesCSV "clientes.csv"
  codigoProduto <- lerCodigo "codigo.csv"
  historicoCompras <- lerHistoricoComprasCSV "historicoCompras.csv"

  initialController produtos clientes codigoProduto historicoCompras




