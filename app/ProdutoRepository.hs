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

-- Função que busca todos os produtos com um atributo específico
buscarProdutosPorAtributo :: Eq a => [Produto] -> (Produto -> a) -> a -> [Produto]
buscarProdutosPorAtributo repositorioProdutos atributo valorDesejado =
  [produto | produto <- repositorioProdutos, atributo produto == valorDesejado]

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

-- Função para altualizar qualquer atributo de um produto especificado
atualizarProdutoPorAtributo :: Eq a => [Produto] -> Int -> (Produto -> a) -> a -> [Produto]
atualizarProdutoPorAtributo [] _ _ _ = []  
atualizarProdutoPorAtributo (produto:produtos) codigoProduto atributo novoValor
  | codigo produto == codigoProduto =
  | otherwise = produto : atualizarProdutoPorAtributo produtos codigoProduto atributo novoValor

