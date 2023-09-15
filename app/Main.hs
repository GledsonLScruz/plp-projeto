module Main where

import Controllers
import Auxiliares

main :: IO ()
main = do
  putStrLn "Bem-vindo ao sistema de Supermercado Online!"

  let produtos = criarRepositorioProdutosExemplo
  let clientes = criarRepositorioClientesExemplo
  let idProduto = 9

  initialController produtos clientes idProduto

