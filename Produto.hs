-- Produto.hs
module Produto where

data Produto = Produto
  { codigo :: Int
  , disponivel :: Bool
  , nome :: String
  , categoria :: String
  , precoCompra :: Double
  , precoVenda :: Double
  , quantidade :: Int
  , fabricacao :: String
  , validade :: String
  } deriving (Show)

-- Funções de acesso para os atributos de Produto
getCodigo :: Produto -> Int
getCodigo = codigo

getDisponivel :: Produto -> Bool
getDisponivel = disponivel

getNome :: Produto -> String
getNome = nome

getCategoria :: Produto -> String
getCategoria = categoria

getPrecoCompra :: Produto -> Double
getPrecoCompra = precoCompra

getPrecoVenda :: Produto -> Double
getPrecoVenda = precoVenda

getQuantidade :: Produto -> Int
getQuantidade = quantidade

getFabricacao :: Produto -> String
getFabricacao = fabricacao

getValidade :: Produto -> String
getValidade = validade

-- Funções de atualização para os atributos de Produto
setCodigo :: Int -> Produto -> Produto
setCodigo novoCodigo produto = produto { codigo = novoCodigo }

setDisponivel :: Bool -> Produto -> Produto
setDisponivel novaDisponibilidade produto = produto { disponivel = novaDisponibilidade }

setNome :: String -> Produto -> Produto
setNome novoNome produto = produto { nome = novoNome }

setCategoria :: String -> Produto -> Produto
setCategoria novaCategoria produto = produto { categoria = novaCategoria }

setPrecoCompra :: Double -> Produto -> Produto
setPrecoCompra novoPreco produto = produto { precoCompra = novoPreco }

setPrecoVenda :: Double -> Produto -> Produto
setPrecoVenda novoPreco produto = produto { precoVenda = novoPreco }

setQuantidade :: Int -> Produto -> Produto
setQuantidade novaQuantidade produto = produto { quantidade = novaQuantidade }

setFabricacao :: String -> Produto -> Produto
setFabricacao novaData produto = produto { fabricacao = novaData }

setValidade :: String -> Produto -> Produto
setValidade novaData produto = produto { validade = novaData }