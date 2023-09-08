module ProdutoRepository where

import Produto

-- Crie uma lista de produtos (um repositório)
criarRepositorioProdutosVazio :: [Produto]
criarRepositorioProdutosVazio = []

-- Adicione um produto ao repositório
adicionarProduto :: [Produto] -> Produto -> [Produto]
adicionarProduto repositorio produto = produto : repositorio

-- Função para buscar um produto por código em uma lista de produtos
buscarProdutoPorCodigo :: [Produto] -> Int -> Maybe Produto
buscarProdutoPorCodigo [] _ = Nothing 
buscarProdutoPorCodigo (produto:produtos) codigo
  | codigo == getCodigo produto = Just produto
  | otherwise = buscarProdutoPorCodigo produtos codigo

-- Função que busca todos os produtos com uma categoria específica
buscarProdutosPorCategoria :: [Produto] -> String -> [Produto]
buscarProdutosPorCategoria repositorioProdutos categoriaBuscada =
  [produto | produto <- repositorioProdutos, categoria produto == categoriaBuscada]

-- Função que remove um produto do repositório
removerProduto :: [Produto] -> Int -> [Produto]
removerProduto repositorio codigoProduto =
  filter (\produto -> codigo produto /= codigoProduto) repositorio

-- Função para atualizar um produto especificado 
atualizarProduto :: [Produto] -> Int -> Produto -> [Produto]
atualizarProduto [] _ _ = []  
atualizarProduto (produto:produtos) codigoProduto novoProduto
  | codigo produto == codigoProduto = do
      novoProduto : produtos  
  | otherwise = produto : atualizarProduto produtos codigoProduto novoProduto


