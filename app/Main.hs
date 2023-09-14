module Main where

import Produto
import Cliente
import ProdutoRepository

main :: IO ()
main = do
  putStrLn "Bem-vindo ao sistema de e-commerce em Haskell!"

  let produtos = criarRepositorioProdutosExemplo
  let clientes = []
  let id = 0 

  admLoop produtos clientes id

-- Loop principal que aguarda comandos do administrador
admLoop :: [Produto] -> [Cliente] -> Int -> IO ()
admLoop produtos clientes id = do
  putStrLn $ "Opções de administrador:\n" ++
           "1. Adicionar Novo Produto\n" ++
           "2. Atualiza Produto por Completo\n" ++
           "3. Ler Produto por Codigo\n" ++
           "4. Ler Produtos por Categoria\n" ++
           "5. Remover Produto por Codigo\n" ++
           "6. Sair do Modo Administrador\n" ++
           "7. Sair do Sistema\n" 

  opcao <- getLine

  case opcao of
    "1" -> do
      novoProduto <- lerProduto id
      let produtosAtualizados = adicionarProduto produtos novoProduto
      putStrLn "Produto adicionado com sucesso."
      admLoop produtosAtualizados clientes (id + 1)

    "2" -> do
      putStrLn "Digite o código do produto a ser atualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      novoProduto <- lerProduto codigoProduto
      let produtosAtualizados = atualizarProduto produtos codigoProduto novoProduto
      putStrLn "Produto atualizado com sucesso."
      admLoop produtosAtualizados clientes id

    "3" -> do
      putStrLn "Digite o código do produto a ser lido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigo produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ "Produto encontrado: " ++ show p
          admLoop produtos clientes id
        Nothing -> do
          putStrLn "Produto não encontrado."
          admLoop produtos clientes id
      
    "4" -> do
      putStrLn "Digite a categoria a ser buscada:"
      categoria <- getLine
      let produtosEncontrados = buscarProdutosPorCategoria produtos categoria
      putStrLn $ "Produtos encontrados: " ++ show produtosEncontrados
      admLoop produtos clientes id

    "5" -> do
      putStrLn "Digite o código do produto a ser removido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produtosAtualizados = removerProduto produtos codigoProduto
      putStrLn "Produto removido com sucesso."
      admLoop produtosAtualizados clientes id

    "6" -> do
      putStrLn "Falta implementar"
      admLoop produtos clientes id

    "7" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      admLoop produtos clientes id

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
