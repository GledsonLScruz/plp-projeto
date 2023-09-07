data Produto = Produto
  { codigo :: Int
  , nome :: String
  , categoria :: String
  , preco :: Double
  , precoVenda :: Double
  , quantidade :: Int
  , fabricacao :: String
  , validade :: String
  } deriving (Show)
