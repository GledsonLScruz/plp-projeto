module Controllers where

import Produto
import Cliente
import ClienteRepository
import ProdutoService
import ClienteService
import Auxiliares
import Tabela
import Data.Char

-- Controller inicial do sistema
initialController :: [Produto] -> [Cliente] -> Int -> IO ()
initialController produtos clientes idProduto = do
  putStrLn $ "Opções de visitante:\n" ++
           "01. Entrar como Cliente\n" ++
           "02. Registrar como Cliente\n" ++
           "03. Entrar como Administrador\n" ++
           "04. Visualizar Produtos\n" ++
           "05. Sair do Sistema\n"

  opcao <- getLine

  case opcao of
    "01" -> loginController produtos clientes idProduto

    "02" -> registrerController produtos clientes idProduto

    "03" -> loginADMController produtos clientes idProduto

    "04" -> do
      mapM_ (putStrLn . produtoToString) produtos
      initialController produtos clientes idProduto

    "05" -> do
      putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      initialController produtos clientes idProduto

-- LoginController guarda os comandos de Login
loginController :: [Produto] -> [Cliente] -> Int -> IO ()
loginController produtos clientes idProduto = do
	putStrLn $ "Faça Login como Cliente:\n" ++ "Email:\n"
	email <- getLine
	putStrLn $ "Senha:\n"
	senha <- getLine
	let usuario = buscarClientePorLogin clientes email senha
	case usuario of
		Just l -> do
			clienteController produtos clientes idProduto l
		Nothing -> do
			putStrLn $ "\nLogin Inválido tente novamente\n"
			initialController produtos clientes idProduto

-- LoginController guarda os comandos de Login
loginADMController :: [Produto] -> [Cliente] -> Int -> IO ()
loginADMController produtos clientes idProduto = do
	putStrLn $ "Faça Login como ADM:\n nome de usuário:\n"
	user <- getLine
	putStrLn $ "Senha:\n"
	senha <- getLine
	if user == "admin" && senha == "password"
		then do
			putStrLn $ "Login como ADM feito com sucesso!"
			admController produtos clientes idProduto
		else do
			putStrLn $ "\nLogin Inválido tente novamente\n"
			loginADMController produtos clientes idProduto

-- RegisterController guarda os comandos de Register
registrerController :: [Produto] -> [Cliente] -> Int -> IO ()
registrerController produtos clientes idProduto = do
	putStrLn $ "Cadastre um novo Cliente:\n"
	novoCliente <- lerCliente
	let clientes = adicionarClienteService clientes novoCliente
	putStrLn $ "Cliente Cadastrado"
	clienteController produtos clientes idProduto novoCliente


-- Controller que guarda comandos do cliente
clienteController :: [Produto] -> [Cliente] -> Int -> Cliente -> IO ()
clienteController produtos clientes idProduto clienteLogado = do
  putStrLn $ "Opções de cliente:\n" ++
           "01. Visualizar Produtos\n" ++
           "02. Visualizar Produtos por Categoria\n" ++
           "03. Adicionar ao Carrinho\n" ++
           "04. Visualizar Carrinho\n" ++
           "05. Finalizar Compra\n" ++
           "06. Avaliar Produto\n" ++
           "07. Histórico de Compra\n" ++
           "08. Atualizar Meu Cadastro\n" ++
           "09. Deletar Minha Conta\n" ++
           "10. Sair do Modo Cliente\n" ++
           "11. Sair do Sistema\n"

  opcao <- getLine

  case opcao of
    "01" -> do
      mapM_ (putStrLn . produtoToString) produtos
      clienteController produtos clientes idProduto clienteLogado

    "02" -> do
        putStrLn "Digite a categoria a ser buscada:"
        categoria <- getLine
        let produtosEncontrados = buscarProdutosPorCategoriaService produtos categoria
        mapM_ (putStrLn . produtoToString) produtosEncontrados
        clienteController produtos clientes idProduto clienteLogado

    "03" -> do
      putStrLn "Falta implementar Adicionar ao Carrinho"
      clienteController produtos clientes idProduto clienteLogado

    "04" -> do
      putStrLn "Falta implementar Visualizar Carrinho"
      clienteController produtos clientes idProduto clienteLogado

    "05" -> do
      putStrLn "Falta implementar Finalizar Compra"
      admController produtos clientes idProduto

    "06" -> do
      putStrLn "Falta implementar Avaliar Produto"
      clienteController produtos clientes idProduto clienteLogado

    "07" -> do
      putStrLn "Falta implementar Histórico de Compra"
      clienteController produtos clientes idProduto clienteLogado

    "08" -> do
      putStrLn "Digite o seu cpf para atualizar o cadastro:"
      cpf <- getLine
      novoCliente <- lerAtualizarCadastro
      let clientesAtualizados = atualizarCadastroClienteService clientes cpf novoCliente
      putStrLn "Cadastro atualizado com sucesso."
      clienteController produtos clientesAtualizados idProduto clienteLogado

    "09" -> do
        putStrLn "Digite o seu cpf para deletar a conta:"
        cpf <- getLine
        let clientesAtualizados = removerClienteService clientes cpf
        putStrLn "Conta deletada com sucesso."
        initialController produtos clientesAtualizados idProduto

    "10" -> do
      initialController produtos clientes idProduto

    "11" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      clienteController produtos clientes idProduto clienteLogado


-- Controller que aguarda comandos do administrador
admController :: [Produto] -> [Cliente] -> Int -> IO ()
admController produtos clientes idProduto = do
  putStrLn $ "Opções de administrador:\n" ++
           "01. Visualizar Produtos\n" ++
           "02. Adicionar Novo Produto\n" ++
           "03. Atualizar Produto por Completo\n" ++
           "04. Visualizar Produto por Codigo\n" ++
           "05. Visualizar Produtos por Categoria\n" ++
           "06. Remover Produto por Codigo\n" ++
           "07. Ler Cliente por CPF\n" ++
           "08. Atualizar Cliente Completo\n" ++
           "09. Deletar Cliente por CPF\n" ++
           "10. Visualizar Dashboard\n" ++
           "11. Sair do Modo Administrador\n" ++
           "12. Sair do Sistema\n"

  opcao <- getLine

  case opcao of
    "01" -> do
      mapM_ (putStrLn . produtoToString) produtos
      admController produtos clientes idProduto

    "02" -> do
      novoProduto <- lerProduto idProduto
      let produtosAtualizados = adicionarProdutoService produtos novoProduto
      putStrLn "Produto adicionado com sucesso."
      salvarProdutos produtosAtualizados
      admController produtosAtualizados clientes (idProduto + 1)

    "03" -> do
      putStrLn "Digite o código do produto a ser atualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      novoProduto <- lerProduto codigoProduto
      let produtosAtualizados = atualizarProdutoService produtos codigoProduto novoProduto
      putStrLn "Produto atualizado com sucesso."
      salvarProdutos produtosAtualizados
      admController produtosAtualizados clientes idProduto

    "04" -> do
      putStrLn "Digite o código do produto a ser visualizado:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produto = buscarProdutoPorCodigoService produtos codigoProduto
      case produto of
        Just p -> do
          putStrLn $ produtoToString p
          admController produtos clientes idProduto
        Nothing -> do
          putStrLn "Produto não encontrado."
          admController produtos clientes idProduto

    "05" -> do
      putStrLn "Digite a categoria a ser buscada:"
      categoria <- getLine
      let produtosEncontrados = buscarProdutosPorCategoriaService produtos categoria
      mapM_ (putStrLn . produtoToString) produtosEncontrados
      admController produtos clientes idProduto

    "06" -> do
      putStrLn "Digite o código do produto a ser removido:"
      codigoProdutoStr <- getLine
      let codigoProduto = read codigoProdutoStr :: Int
      let produtosAtualizados = removerProdutoService produtos codigoProduto
      putStrLn "Produto removido com sucesso."
      salvarProdutos produtosAtualizados
      admController produtosAtualizados clientes idProduto

    "07" -> do
        putStrLn "Digite o CPF do cliente a ser buscado:"
        cpfCliente <- getLine
        let cliente = buscarClientePorCpfService clientes cpfCliente
        case cliente of
            Just c -> do
                putStrLn $ clienteToString c
                admController produtos clientes idProduto
            Nothing -> do
                putStrLn "Cliente não encontrado."
                admController produtos clientes idProduto

    "08" -> do
        putStrLn "Digite o CPF do cliente a ser atualizado:"
        cpfCliente <- getLine
        novoCliente <- lerCliente
        let clientesAtualizados = atualizarClienteService clientes cpfCliente novoCliente
        putStrLn "Cliente atualizado com sucesso."
        salvarClientes clientesAtualizados
        admController produtos clientes idProduto

    "09" -> do
        putStrLn "Digite o CPF do cliente a ser removido:"
        cpfCliente <- getLine
        let clientesAtualizados = removerClienteService clientes cpfCliente
        putStrLn "Cliente removido com sucesso."
        salvarClientes clientesAtualizados
        admController produtos clientes idProduto

    "10" -> do
      putStrLn "Falta implementar o Dashboard"
      admController produtos clientes idProduto

    "11" -> do
      initialController produtos clientes idProduto

    "12" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      admController produtos clientes idProduto
