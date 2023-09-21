module TaskCarrinho where

import Produto
-- Definindo o tipo de dados para carrinho de compras
type CarrinhoCompra = [Produto]

{-
-- Função para adicionar um produto ao carrinho de compras
adicionarProduto :: Produto -> CarrinhoCompra -> CarrinhoCompra
adicionarProduto produto carrinho = produto : carrinho

-- Função para visualizar o carrinho de compras
visualizarCarrinho :: CarrinhoCompra -> IO ()
visualizarCarrinho carrinho = do
    putStrLn "Carrinho de Compras:"
    mapM_ (putStrLn . formatarProduto) carrinho
    putStrLn $ "Total: R$ " ++ show (calcularTotal carrinho)

-- Função para calcular o total do carrinho de compras
calcularTotal :: CarrinhoCompra -> Double
calcularTotal = sum . map precoProduto

-- Função para avaliar um produto
avaliarProduto :: Produto -> Double -> Produto
avaliarProduto produto avaliacao = produto { avaliacaoProduto = Just avaliacao }

-- Função para fazer compras
fazerCompras :: CarrinhoCompra -> IO CarrinhoCompra
fazerCompras carrinho = do
    putStrLn "O que você gostaria de comprar?"
    nome <- getLine
    putStrLn "Qual é o preço?"
    preco <- readLn :: IO Double
    let produto = Produto nome preco Nothing
    putStrLn "Produto adicionado ao carrinho."
    putStrLn "Deseja comprar mais produtos? (S/N)"
    resposta <- getLine
    if resposta == "S" || resposta == "s"
        then do
            proximoCarrinho <- fazerCompras (adicionarProduto produto carrinho)
            return proximoCarrinho
        else
            return (adicionarProduto produto carrinho)

-- Função para formatar um produto
formatarProduto :: Produto -> String
formatarProduto produto =
    nomeProduto produto ++
    " - R$ " ++ show (precoProduto produto) ++
    case avaliacaoProduto produto of
        Just avaliacao -> " (Avaliação: " ++ show avaliacao ++ ")"
        Nothing -> ""
-}