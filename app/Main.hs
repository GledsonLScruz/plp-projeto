module Main where

import Produto
import Cliente
import ProdutoRepository
import ProdutoService
import Auxiliares

main :: IO ()
main = do
  putStrLn "Bem-vindo ao sistema de e-commerce em Haskell!"

  let produtos = criarRepositorioProdutosExemplo
  let clientes = []
  let id = 9 

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
      let produtosAtualizados = adicionarProdutoService produtos novoProduto
      putStrLn "Produto adicionado com sucesso."
      admLoop produtosAtualizados clientes (id + 1)

    "2" -> do
      putStrLn "Digite o código do produto a ser atualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      novoProduto <- lerProduto codigoProduto
      let produtosAtualizados = atualizarProdutoService produtos codigoProduto novoProduto
      putStrLn "Produto atualizado com sucesso."
      admLoop produtosAtualizados clientes id

    "3" -> do
      putStrLn "Digite o código do produto a ser lido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigoService produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ produtoToString p
          admLoop produtos clientes id
        Nothing -> do
          putStrLn "Produto não encontrado."
          admLoop produtos clientes id
      
    "4" -> do
      putStrLn "Digite a categoria a ser buscada:"
      categoria <- getLine
      let produtosEncontrados = buscarProdutosPorCategoriaService produtos categoria
      putStrLn $ "Produtos encontrados: " ++ show produtosEncontrados
      admLoop produtos clientes id

    "5" -> do
      putStrLn "Digite o código do produto a ser removido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produtosAtualizados = removerProdutoService produtos codigoProduto
      putStrLn "Produto removido com sucesso."
      admLoop produtosAtualizados clientes id

    "6" -> do
      putStrLn "Falta implementar"
      admLoop produtos clientes id

    "7" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      admLoop produtos clientes id

