module CarrinhoService where

import Produto
-- Definindo o tipo de dados para carrinho de compras
data CarrinhoCompra = CarrinhoCompra
  { produtos :: [Produto]
  } deriving (Show)


-- Função para adicionar um produto ao carrinho de compras
adicionarProduto :: CarrinhoCompra -> Produto -> CarrinhoCompra
adicionarProduto carrinho produto = carrinho { produtos = produtos carrinho ++ [produto]}

printProduto :: Produto -> String
printProduto produto =
	"Produto: " ++ nome produto ++ " ---------" ++ " Valor em R$" ++ show (precoCompra produto)

-- Função para calcular o total do carrinho de compras
calcularTotal :: CarrinhoCompra -> Double
calcularTotal carrinho = sumList carrinho


sumList :: CarrinhoCompra -> Double
sumList carrinho = do
	let produtos = produtos carrinho
sumList produtos carrinho == [] = 0.0
sumList (x:xs) = precoCompra x + sumList xs
