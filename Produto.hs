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
getCodigo (Produto cod _ _ _ _ _ _ _ _) = cod

getDisponivel :: Produto -> Bool
getDisponivel (Produto _ dis _ _ _ _ _ _ _) = dis

getNome :: Produto -> String
getNome (Produto _ _ nom _ _ _ _ _ _) = nom

getCategoria :: Produto -> String
getCategoria (Produto _ _ _ cat _ _ _ _ _) = cat

getPrecoCompra :: Produto -> Double
getPrecoCompra (Produto _ _ _ _ pc _ _ _ _) = pc

getPrecoVenda :: Produto -> Double
getPrecoVenda (Produto _ _ _ _ _ pv _ _ _) = pv

getQuantidade :: Produto -> Int
getQuantidade (Produto _ _ _ _ _ _ qt _ _) = qt

getFabricacao :: Produto -> String
getFabricacao (Produto _ _ _ _ _ _ _ fab _) = fab

getValidade :: Produto -> String
getValidade (Produto _ _ _ _ _ _ _ _ val) = val

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