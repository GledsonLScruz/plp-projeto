module ProdutoRepository
  ( ProdutoRepository(..)
  , emptyRepository
  , adicionarProduto
  , buscarProdutoPorCodigo
  , removerProdutoPorCodigo
  , listarProdutos
  ) where

import Data.Map (Map)
import Produto
import qualified Data.Map as Map

newtype ProdutoRepository = ProdutoRepository { 
    produtos :: Map Int Produto
  } deriving (Show)

-- Cria um repositório vazio
emptyRepository :: ProdutoRepository
emptyRepository = ProdutoRepository { produtos = Map.empty }

-- Adiciona um produto ao repositório
adicionarProduto :: Produto -> ProdutoRepository -> ProdutoRepository
adicionarProduto produto repo =
  repo { produtos = Map.insert (codigo produto) produto (produtos repo) }

-- Busca um produto pelo código
buscarProdutoPorCodigo :: Int -> ProdutoRepository -> Maybe Produto
buscarProdutoPorCodigo codigoProduto repo = Map.lookup codigoProduto (produtos repo)

-- Remove um produto pelo código
removerProdutoPorCodigo :: Int -> ProdutoRepository -> ProdutoRepository
removerProdutoPorCodigo codigoProduto repo =
  repo { produtos = Map.delete codigoProduto (produtos repo) }

-- Lista todos os produtos no repositório
listarProdutos :: ProdutoRepository -> [Produto]
listarProdutos repo = Map.elems (produtos repo)
