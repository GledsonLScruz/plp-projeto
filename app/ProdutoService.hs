module ProdutoService where

import Produto
import ProdutoRepository

-- Função para adicionar um novo produto à lista de produtos
adicionarProdutoService :: [Produto] -> Produto -> [Produto]
adicionarProdutoService produtos novoProduto = adicionarProduto produtos novoProduto

-- Função para atualizar um produto da lista de produtos
atualizarProdutoService :: [Produto] -> Int -> Produto -> [Produto]
atualizarProdutoService produtos codigoProduto novoProduto = atualizarProduto produtos codigoProduto novoProduto

-- Função para buscar um produto da lista de produtos
buscarProdutoPorCodigoService :: [Produto] -> Int -> Maybe Produto
buscarProdutoPorCodigoService produtos codigoProduto = buscarProdutoPorCodigo produtos codigoProduto

-- Função para buscar produtos por categoria
buscarProdutosPorCategoriaService :: [Produto] -> String -> [Produto]
buscarProdutosPorCategoriaService produtos categoria = buscarProdutosPorCategoria produtos categoria

-- Função para remover um produto da lista de produtos
removerProdutoService :: [Produto] -> Int -> [Produto]
removerProdutoService produtos codigoProduto = removerProduto produtos codigoProduto