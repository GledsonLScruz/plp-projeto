module CarrinhoService where

import Produto

-- Definindo o tipo de dados para carrinho de compras
data CarrinhoCompra = CarrinhoCompra
  { produtos :: [Produto]
  } deriving (Show)

getCarrinhoProdutos :: CarrinhoCompra -> [Produto]
getCarrinhoProdutos carrinho = produtos carrinho


-- Função para adicionar um produto ao carrinho de compras
adicionarProdutoCarrinho :: CarrinhoCompra -> Produto -> CarrinhoCompra
adicionarProdutoCarrinho carrinho produto
  | getDisponivel produto = carrinho { produtos = produto : produtos carrinho }
  | otherwise = carrinho
  

-- Função para remover um produto do carrinho de compras com base no código
removerProdutoCarrinho :: CarrinhoCompra -> Int -> CarrinhoCompra
removerProdutoCarrinho carrinho codigoProduto =
    carrinho { produtos = filter (\produto -> getCodigo produto /= codigoProduto) (produtos carrinho) }


calcularTotal :: CarrinhoCompra -> Double
calcularTotal carrinho = sumList (produtos carrinho)

sumList :: [Produto] -> Double
sumList [] = 0.0
sumList (x:xs) = precoCompra x + sumList xs
