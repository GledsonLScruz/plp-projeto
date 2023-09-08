module ProdutoRepository where
import Produto

-- Crie uma lista de produtos (um repositório)
criarRepositorioProdutosVazio :: [Produto]
criarRepositorioProdutosVazio = []

-- Adicione um produto ao repositório
adicionarProduto :: [Produto] -> Produto -> [Produto]
adicionarProduto repositorio produto = produto : repositorio

-- Remova um produto do repositório
removerProduto :: [Produto] -> String -> [Produto]
removerProduto repositorio codigoProduto =
  filter (\produto -> codigo produto /= codigoProduto) repositorio

-- Função para alterar qualquer atributo de um produto especificado
alterarAtributoProduto :: [Produto] -> String -> (Produto -> Produto) -> [Produto]
alterarAtributoProduto [] _ _ = []  
alterarAtributoProduto (produto:produtos) codigoProduto alteracao
  | codigo produto == codigoProduto = alteracao produto : produtos
  | otherwise = produto : alterarAtributoProduto produtos codigoProduto alteracao

-- Funções para atualizar um produto especificado 
atualizarProduto :: [Produto] -> String -> Bool -> String -> String -> Double -> Double -> Int -> String -> String -> [Produto]
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


-- Exemplo de list comprehension: obter todos os produtos de uma categoria específica
produtosDaCategoria :: [Produto] -> String -> [Produto]
produtosDaCategoria repositorioProdutos categoriaDesejada =
  [ produto | produto <- repositorioProdutos, categoria produto == categoriaDesejada ]

