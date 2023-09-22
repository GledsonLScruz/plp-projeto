module Tabela where

import System.IO
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Produto
import Cliente

-- Função para converter um Produto em uma linha CSV
instance ToRecord Produto where
  toRecord p = record
    [ toField (Produto.codigo p)
    , toField (boolToString (Produto.disponivel p)) -- Converte Bool para String
    , toField (Produto.nome p)
    , toField (Produto.categoria p)
    , toField (Produto.precoCompra p)
    , toField (Produto.precoVenda p)
    , toField (Produto.quantidade p)
    , toField (Produto.fabricacao p)
    , toField (Produto.validade p)
    ]

-- Função para converter um Cliente em uma linha CSV
instance ToRecord Cliente where
  toRecord c = record
    [ toField (Cliente.nomeCompleto c)
    , toField (Cliente.sexo c)
    , toField (Cliente.dataNascimento c)
    , toField (Cliente.cpf c)
    , toField (Cliente.email c)
    , toField (Cliente.telefone c)
    , toField (Cliente.nomeUsuario c)
    , toField (Cliente.senha c)

    ]

-- Função para salvar uma lista de produtos em um arquivo CSV
salvarProdutos :: [Produto] -> IO ()
salvarProdutos produtos = do
  let csvData = encode produtos
  BL.writeFile "produtos.csv" csvData
  

-- Função para salvar uma lista de clientes em um arquivo CSV
salvarClientes :: [Cliente] -> IO ()
salvarClientes clientes = do
  let csvData = encode clientes
  BL.writeFile "clientes.csv" csvData
  

-- Função para converter uma linha CSV em um Produto
instance FromRecord Produto where
  parseRecord v
    | length v == 9 = Produto
      <$> v .! 0  -- Código
      <*> (stringToBool <$> v .! 1) -- Disponível
      <*> v .! 2  -- Nome
      <*> v .! 3  -- Categoria
      <*> v .! 4  -- Preço de Compra
      <*> v .! 5  -- Preço de Venda
      <*> v .! 6  -- Quantidade
      <*> v .! 7  -- Fabricação
      <*> v .! 8  -- Validade
    | otherwise = fail "Número incorreto de campos ao decodificar Produto"


-- Função para converter uma linha CSV em um Produto
instance FromRecord Cliente where
  parseRecord v
    | length v == 8 = Cliente
      <$> v .! 0  -- Nome Completo
      <*> v .! 1  -- Sexo
      <*> v .! 2  -- Data de Nascimento
      <*> v .! 3  -- CPF
      <*> v .! 4  -- E-mail
      <*> v .! 5  -- Telefone
      <*> v .! 6  -- Nome de Usuário
      <*> v .! 7  -- Senha
    | otherwise = fail "Número incorreto de campos ao decodificar Cliente"


-- Função para ler um arquivo CSV de produtos e converter em uma lista
lerProdutosCSV :: FilePath -> IO [Produto]
lerProdutosCSV filePath = do
  csvData <- BL.readFile filePath
  case decode NoHeader csvData of
    Left err -> do
      putStrLn $ "Erro ao ler o arquivo CSV: " ++ err
      return []
    Right rows -> return $ V.toList rows

-- Função para ler um arquivo CSV de clientes e converter em uma lista
lerClientesCSV :: FilePath -> IO [Cliente]
lerClientesCSV filePath = do
  csvData <- BL.readFile filePath
  case decode NoHeader csvData of
    Left err -> do
      putStrLn $ "Erro ao ler o arquivo CSV: " ++ err
      return []
    Right rows -> return $ V.toList rows

-- Função auxiliar para converter Bool em String
boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

-- Função auxiliar para converter String em Bool
stringToBool :: String -> Bool
stringToBool "True" = True
stringToBool "False" = False
stringToBool _ = False  

-- Função para salvar o valor inteiro em um arquivo CSV
salvarCodigo :: Int -> IO ()
salvarCodigo valor = do
  let csvData = encode [Only valor] 
  BL.writeFile "codigo.csv" csvData

lerCodigo :: String -> IO Int
lerCodigo path = do
  handle <- openFile path ReadMode
  conteudo <- hGetContents handle
  let meuID = read conteudo :: Int
  hClose handle
  return meuID
 
salvarHistoricoCompras :: [Produto] -> IO ()
salvarHistoricoCompras historicoCompras = do
  let csvData = encode historicoCompras
  BL.writeFile "historicoCompras.csv" csvData

lerHistoricoComprasCSV :: FilePath -> IO [Produto]
lerHistoricoComprasCSV filePath = do
  csvData <- BL.readFile filePath
  case decode NoHeader csvData of
    Left err -> do
      putStrLn $ "Erro ao ler o arquivo CSV: " ++ err
      return []
    Right rows -> return $ V.toList rows