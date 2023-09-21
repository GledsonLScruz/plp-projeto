module CarrinhoService where

import Produto

-- Definindo o tipo de dados para carrinho de compras
data CarrinhoCompra = CarrinhoCompra
  { produtos :: [Produto]
  } deriving (Show)

getCarrinhoProdutos :: CarrinhoCompra -> [Produto]
getCarrinhoProdutos carrinho = produtos carrinho

-- Função para adicionar um produto ao carrinho de compras
adicionarProduto :: CarrinhoCompra -> Produto -> CarrinhoCompra
adicionarProduto carrinho produto = 
    carrinho { produtos = produto : produtos carrinho }

calcularTotal :: CarrinhoCompra -> Double
calcularTotal carrinho = sumList (produtos carrinho)

sumList :: [Produto] -> Double
sumList [] = 0.0
sumList (x:xs) = precoCompra x + sumList xs
