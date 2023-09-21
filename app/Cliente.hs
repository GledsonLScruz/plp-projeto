module Cliente where

data Cliente = Cliente
  { nomeCompleto :: String
  , sexo :: Char
  , dataNascimento :: String
  , cpf :: String
  , email :: String
  , telefone :: String
  , nomeUsuario :: String
  , senha :: String
  } deriving (Show)

-- Funções de acesso para os atributos de Cliente
getNomeCompleto :: Cliente -> String
getNomeCompleto = nomeCompleto

getSexo :: Cliente -> Char
getSexo = sexo

getDataNascimento :: Cliente -> String
getDataNascimento = dataNascimento

getCpf :: Cliente -> String
getCpf = cpf

getEmail :: Cliente -> String
getEmail = email

getTelefone :: Cliente -> String
getTelefone = telefone

getNomeUsuario :: Cliente -> String
getNomeUsuario = nomeUsuario

getSenha :: Cliente -> String
getSenha = senha

-- Funções de atualização para os atributos de Cliente
setNomeCompleto :: String -> Cliente -> Cliente
setNomeCompleto novoNome cliente = cliente { nomeCompleto = novoNome }

setSexo :: Char -> Cliente -> Cliente
setSexo novoSexo cliente = cliente { sexo = novoSexo }

setDataNascimento :: String -> Cliente -> Cliente
setDataNascimento novaData cliente = cliente { dataNascimento = novaData }

setCPF :: String -> Cliente -> Cliente
setCPF novoCPF cliente = cliente { cpf = novoCPF }

setEmail :: String -> Cliente -> Cliente
setEmail novoEmail cliente = cliente { email = novoEmail }

setTelefone :: String -> Cliente -> Cliente
setTelefone novoTelefone cliente = cliente { telefone = novoTelefone }

setNomeUsuario :: String -> Cliente -> Cliente
setNomeUsuario novoNomeUsuario cliente = cliente { nomeUsuario = novoNomeUsuario }

setSenha :: String -> Cliente -> Cliente
setSenha novaSenha cliente = cliente { senha = novaSenha }

clienteToString :: Cliente -> String
clienteToString cliente =
  "----------------------------------------\n" ++
  "Nome Completo: " ++ nomeCompleto cliente ++ "\n" ++
  "Sexo: " ++ [sexo cliente] ++ "\n" ++
  "Data de Nascimento: " ++ dataNascimento cliente ++ "\n" ++
  "CPF: " ++ cpf cliente ++ "\n" ++
  "Email: " ++ email cliente ++ "\n" ++
  "Telefone: " ++ telefone cliente ++ "\n" ++
  "Nome de Usuário: " ++ nomeUsuario cliente ++ "\n" ++
  "Senha: " ++ senha cliente ++ "\n" ++
  "----------------------------------------"
