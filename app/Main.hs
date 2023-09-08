module Main where

import Produto
import Cliente
import ProdutoRepository

main :: IO ()
main = do
  putStrLn "Bem-vindo ao sistema de e-commerce em Haskell!"

  -- Exemplo de loop principal que aguarda comandos do usuário
  mainLoop [] []

-- Loop principal que aguarda comandos do usuário
mainLoop :: [Produto] -> [Cliente] -> IO ()
mainLoop produtos clientes = do
  putStrLn "Escolha uma opção:"
  putStrLn "1. Adicionar Produto"
  putStrLn "2. Atualizar Estoque"
  putStrLn "3. Realizar Compra"
  putStrLn "4. Gerar Relatório"
  putStrLn "5. Sair"

  opcao <- getLine

  case opcao of
    "1" -> do
      novoProduto <- lerProduto
      -- let produtosAtualizados = adicionarProduto produtos novoProduto
      putStrLn "Produto adicionado com sucesso."
      mainLoop produtos clientes

    "2" -> do
      putStrLn "Digite o código do produto a ser atualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      putStrLn "Digite a nova quantidade:"
      quantidadeStr <- getLine
      let novaQuantidade = read quantidadeStr :: Int
      -- let produtosAtualizados = atualizarEstoque produtos codigoProduto novaQuantidade
      putStrLn "Estoque atualizado com sucesso."
      mainLoop produtos clientes

    "3" -> do
      -- Implemente a lógica para realizar uma compra
      -- Lembre-se de lidar com carrinhos, avaliações, etc.
      putStrLn "Funcionalidade de compra ainda não implementada."
      mainLoop produtos clientes

    "4" -> do
      -- Implemente a lógica para gerar relatórios
      putStrLn "Funcionalidade de geração de relatórios ainda não implementada."
      mainLoop produtos clientes

    "5" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      mainLoop produtos clientes

-- Função auxiliar para ler um novo produto do usuário
lerProduto :: IO Produto
lerProduto = do
  putStrLn "Digite o nome do produto:"
  nomeProduto <- getLine
  putStrLn "Digite o código do produto:"
  codigoProdutoStr <- getLine
  let codigoProduto = read codigoProdutoStr 
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
  return
    Produto
      { codigo = codigoProduto
      , disponivel = True
      , nome = nomeProduto
      , categoria = categoriaProduto
      , precoCompra = precoProduto
      , precoVenda = precoVendaProduto
      , quantidade = quantidadeProduto
      , fabricacao = dataFabricacao
      , validade = dataValidade
      }
