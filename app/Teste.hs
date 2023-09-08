module Teste where

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

data Produto = Produto
  { codigo :: String
  , disponivel :: Bool
  , nome :: String
  , categoria :: String
  , precoCompra :: Double
  , precoVenda :: Double
  , quantidade :: Int
  , fabricacao :: String
  , validade :: String
  } deriving (Show)

main :: IO ()
main = do
  let produtos = [ Produto "123" True "Arroz" "Alimentos" 5.99 8.99 100 "01/01/2023" "01/01/2024"
                 , Produto "456" True "Feijão" "Alimentos" 4.99 7.99 80 "01/01/2023" "01/01/2024"
                 , Produto "789" True "Sabonete" "Higiene" 2.49 4.99 120 "01/01/2023" "01/01/2024"
                 ]

  putStrLn "Lista de produtos:"
  mapM_ print produtos

  putStrLn "\nDigite o código do produto que deseja atualizar:"
  codigoProduto <- getLine

  putStrLn "Digite a nova disponibilidade do produto (True or False):"
  novaDisponibilidade <- getLine
  let newDisponivel = read novaDisponibilidade :: Bool

  putStrLn "Digite o novo nome do produto:"
  novoNome <- getLine

  putStrLn "Digite a nova categoria do produto:"
  novaCategoria <- getLine

  putStrLn "Digite o novo preço de compra do produto:"
  novoPrecoCompraStr <- getLine
  let novoPrecoCompra = read novoPrecoCompraStr :: Double

  putStrLn "Digite o novo preço de venda do produto:"
  novoPrecoVendaStr <- getLine
  let novoPrecoVenda = read novoPrecoVendaStr :: Double

  putStrLn "Digite a nova quantidade do produto:"
  novaQuantidadeStr <- getLine
  let novaQuantidade = read novaQuantidadeStr :: Int

  putStrLn "Digite a nova data de fabricação do produto:"
  novaFabricacao <- getLine

  putStrLn "Digite a nova data de validade do produto:"
  novaValidade <- getLine

  let produtosAtualizados = atualizarProduto codigoProduto newDisponivel novoNome novaCategoria novoPrecoCompra novoPrecoVenda novaQuantidade novaFabricacao novaValidade
  
  putStrLn "\nLista de produtos atualizada:"
  mapM_ print produtosAtualizados
