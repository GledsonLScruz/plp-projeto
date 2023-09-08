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

-- Remova um produto do repositório
removerProduto :: [Produto] -> Int -> [Produto]
removerProduto repositorio codigoProduto =
  filter (\produto -> codigo produto /= codigoProduto) repositorio

-- Função para alterar qualquer atributo de um produto especificado
alterarAtributoProduto :: [Produto] -> Int -> (Produto -> Produto) -> [Produto]
alterarAtributoProduto [] _ _ = []  
alterarAtributoProduto (produto:produtos) codigoProduto alteracao
  | codigo produto == codigoProduto = alteracao produto : produtos
  | otherwise = produto : alterarAtributoProduto produtos codigoProduto alteracao

-- Funções para atualizar um produto especificado 
atualizarProduto :: [Produto] -> Int -> Bool -> String -> String -> Double -> Double -> Int -> String -> String -> [Produto]
atualizarProduto [] _ _ _ _ _ _ _ _ _ = []  
atualizarProduto (produto:produtos) codigoProduto newDisponivel newNome newCategoria newPrecoCompra newPrecoVenda newQuantidade newFabricacao newValidade
  | codigo produto == codigoProduto = do
      let produtoAtualizado = Produto
            { codigo = codigoProduto
            , disponivel = newDisponivel
            , nome = newNome
            , categoria = newCategoria
            , precoCompra = newPrecoCompra
            , precoVenda = newPrecoVenda
            , quantidade = newQuantidade
            , fabricacao = newFabricacao
            , validade = newValidade
            }
      produtoAtualizado : produtos  
  | otherwise = produto : atualizarProduto produtos codigoProduto newDisponivel newNome newCategoria newPrecoCompra newPrecoVenda newQuantidade newFabricacao newValidade


-- Obter todos os produtos com um atributo específico
produtosPorAtributo :: Eq a => [Produto] -> (Produto -> a) -> a -> [Produto]
produtosPorAtributo repositorioProdutos atributoDesejado valorDesejado =
  [produto | produto <- repositorioProdutos, atributoDesejado produto == valorDesejado]


