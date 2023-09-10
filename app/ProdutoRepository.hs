module ProdutoRepository where

import Produto

-- Crie uma lista de produtos (um repositório)
criarRepositorioProdutosPadrao :: [Produto]
criarRepositorioProdutosPadrao = [
  Produto {codigo = 1, disponivel = True, nome = "Arroz", categoria = "Alimento", precoCompra = 10.0, precoVenda = 15.0, quantidade = 100, fabricacao = "01/01/2021", validade = "01/01/2022"}, 
  Produto {codigo = 2, disponivel = True, nome = "Feijão", categoria = "Alimento", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"}, 
  Produto {codigo = 3, disponivel = True, nome = "Coca-Cola", categoria = "Bebida", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"}, 
  Produto {codigo = 4, disponivel = True, nome = "Pepsi", categoria = "Bebida", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 5, disponivel = True, nome = "Sabão em pó", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 6, disponivel = True, nome = "Sabão em barra", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 7, disponivel = True, nome = "Detergente", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"},
  Produto {codigo = 8, disponivel = True, nome = "Desinfetante", categoria = "Limpeza", precoCompra = 5.0, precoVenda = 10.0, quantidade = 50, fabricacao = "01/01/2021", validade = "01/01/2022"}]

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


