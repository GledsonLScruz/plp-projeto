module Auxiliares where

import Produto

-- Função auxiliar para ler um novo produto do usuário
lerProduto :: Int -> IO Produto
lerProduto idSistema = do
  putStrLn "Digite o nome do produto:"
  nomeProduto <- getLine
  putStrLn "Digite a categoria do produto:"
  categoriaProduto <- getLine
  putStrLn "Digite o preço do produto:"
  precoStr <- getLine
  let precoProduto = read precoStr :: Double
  putStrLn "Digite o preço de venda do produto:"
  precoVendaStr <- getLine
  let precoVendaProduto = read precoVendaStr :: Double
  putStrLn "Digite a quantidade inicial em estoque:"
  quantidadeStr <- getLine
  let quantidadeProduto = read quantidadeStr :: Int
  putStrLn "Digite a data de fabricação do produto:"
  dataFabricacao <- getLine
  putStrLn "Digite a data de validade do produto:"
  dataValidade <- getLine
  let id = idSistema
  return
    Produto
      { codigo = id
      , disponivel = True
      , nome = nomeProduto
      , categoria = categoriaProduto
      , precoCompra = precoProduto
      , precoVenda = precoVendaProduto
      , quantidade = quantidadeProduto
      , fabricacao = dataFabricacao
      , validade = dataValidade
      }