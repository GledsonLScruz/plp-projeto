-- Produto.hs
module Produto where

data Produto = Produto
  { codigo :: Int
  , nome :: String
  , categoria :: String
  , precoCompra :: Double
  , precoVenda :: Double
  , quantidade :: Int
  , fabricacao :: String
  , validade :: String
  } deriving (Show)
