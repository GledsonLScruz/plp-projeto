module AvaliacaoProduto where

data AvaliacaoProduto = AvaliacaoProduto {
	userCPF :: String
	, produtoCode :: Int
	, nota :: Double
	, descricaoAvaliacao :: String
} deriving (Show)

getNota :: AvaliacaoProduto -> Double
getNota = nota

getUserCPF :: AvaliacaoProduto -> String
getUserCPF = userCPF

getDescricaoAvaliacao :: AvaliacaoProduto -> String
getDescricaoAvaliacao = descricaoAvaliacao

setNota :: AvaliacaoProduto -> Double -> AvaliacaoProduto
setNota avaliacao novaNota = avaliacao { nota = novaNota}

setDescricao :: AvaliacaoProduto -> String -> AvaliacaoProduto
setDescricao avaliacao descricao = avaliacao { descricaoAvaliacao = descricao}

avaliacaoToString :: AvaliacaoProduto -> String
avaliacaoToString avaliacao =
  "----------------------------------------\n" ++
  "Nota: " ++ show( nota avaliacao )  ++ "\n" ++
  "Avaliação: " ++ descricaoAvaliacao avaliacao ++ "\n" ++
  "----------------------------------------"