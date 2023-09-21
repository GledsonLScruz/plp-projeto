module ProdutoRepository where

import Data.List (find)
import Produto
import Data.Maybe (fromMaybe)

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


-- Função para remover uma unidade de um produto do repositório
removerUnidadesProduto :: [Produto] -> [Produto] -> [Produto]
removerUnidadesProduto estoque carrinho =
  let removeUnidade p = fromMaybe p (diminuirQuantidadeProduto p)
  in map removeUnidade estoque
  where
    diminuirQuantidadeProduto :: Produto -> Maybe Produto
    diminuirQuantidadeProduto p
      | quantidade p > 0 =
        Just $ setQuantidade (quantidade p - 1) p
      | otherwise =
        Just $ setQuantidade 0 $ setDisponivel False p
