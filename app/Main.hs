module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv

import Controllers
import Auxiliares
import Tabela

main :: IO ()
main = do
  putStrLn "Bem-vindo ao sistema de Supermercado Online!"

  produtos <- lerProdutosCSV "produtos.csv"
  clientes <- lerClientesCSV "clientes.csv"
  let idProduto = 9

  initialController produtos clientes idProduto




