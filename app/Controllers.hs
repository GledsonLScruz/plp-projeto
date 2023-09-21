module Controllers where

import Produto
import Cliente
import ClienteRepository
import ProdutoService
import ClienteService
import Auxiliares
import Tabela
import Dashboards
import Data.Char
import CarrinhoService

-- Controller inicial do sistema
initialController :: [Produto] -> [Cliente] -> Int -> IO ()
initialController produtos clientes idProduto = do
  putStrLn $
    "=====================================\n" ++
    "         Menu de Visitante          \n" ++
    "=====================================\n\n" ++
    "  (01) Entrar como Cliente\n" ++
    "  (02) Registrar como Cliente\n" ++
    "  (03) Entrar como Administrador\n" ++
    "  (04) Visualizar Produtos\n" ++
    "  (05) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
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
  putStrLn $ "Faça Login como Cliente:\nEmail:\n"
  email <- getLine
  putStrLn $ "Senha:\n"
  senha <- getLine
  carrinhoVazio <- novoCarinhoVazio
  let usuario = buscarClientePorLogin clientes email senha
  case usuario of
    Just l -> do
      clienteController produtos clientes idProduto l carrinhoVazio
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
  carrinhoVazio <- novoCarinhoVazio
  clienteController produtos clientes idProduto novoCliente carrinhoVazio


-- Controller que guarda comandos do cliente
clienteController :: [Produto] -> [Cliente] -> Int -> Cliente -> CarrinhoCompra -> IO ()
clienteController produtos clientes idProduto clienteLogado -> carrinho = do
  putStrLn $
    "=====================================\n" ++
    "        Menu de Cliente             \n" ++
    "=====================================\n\n" ++
    "  (01) Visualizar Produtos\n" ++
    "  (02) Visualizar Produtos por Categoria\n" ++
    "  (03) Adicionar ao Carrinho\n" ++
    "  (04) Visualizar Carrinho\n" ++
    "  (05) Finalizar Compra\n" ++
    "  (06) Avaliar Produto\n" ++
    "  (07) Histórico de Compra\n" ++
    "  (08) Atualizar Meu Cadastro\n" ++
    "  (09) Deletar Minha Conta\n" ++
    "  (10) Sair do Modo Cliente\n" ++
    "  (11) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
  opcao <- getLine

  case opcao of
    "01" -> do
      mapM_ (putStrLn . produtoToString) produtos
      clienteController produtos clientes idProduto clienteLogado carrinho

    "02" -> do
        putStrLn "Digite a categoria a ser buscada:"
        categoria <- getLine
        let produtosEncontrados = buscarProdutosPorCategoriaService produtos categoria
        mapM_ (putStrLn . produtoToString) produtosEncontrados
        clienteController produtos clientes idProduto clienteLogado carrinho

    "03" -> do
      carrinhoController produtos clientes idProduto clienteLogado carrinho

    "04" -> do
		putStrLn "Carrinho de Compras:"
        mapM_ (putStrLn . printProduto) produtos carrinho
		clienteController produtos clientes idProduto clienteLogado carrinho

    "05" -> do
      putStrLn "Falta implementar Finalizar Compra"
      clienteController produtos clientes idProduto clienteLogado carrinho

    "06" -> do
      putStrLn "Falta implementar Avaliar Produto"
      clienteController produtos clientes idProduto clienteLogado carrinho

    "07" -> do
      putStrLn "Falta implementar Histórico de Compra"
      clienteController produtos clientes idProduto clienteLogado carrinho

    "08" -> do
      putStrLn "Digite o seu cpf para atualizar o cadastro:"
      cpf <- getLine
      novoCliente <- lerAtualizarCadastro
      let clientesAtualizados = atualizarCadastroClienteService clientes cpf novoCliente
      putStrLn "Cadastro atualizado com sucesso."
      clienteController produtos clientesAtualizados idProduto clienteLogado carrinho

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
      clienteController produtos clientes idProduto clienteLogado carrinho


carrinhoController produtos clientes idProduto clienteLogado carrinho
carrinhoController :: [Produto] -> [Cliente] -> Int -> Cliente -> CarrinhoCompra -> IO ()
carrinhoController produtos clientes clienteLogado carrinho = do
	putStrLn $ "Insira o codigo do produto que você deseja adicionar ao Carrinho: \n"
	codigo <- readLn
	produto <- buscarProdutoPorCodigo codigo
	case produto of
        Just p -> do
          putStrLn $ "O produto:\n" ++ produtoToString p ++ "\nFoi Adicionado ao Carrinho!!\n"
		  carrinho <- adicionarProduto p carrinho
		  clienteController produtos clientes idProduto clienteLogado carrinho
        Nothing -> do
          putStrLn "Produto não encontrado."
          clienteController produtos clientes idProduto clienteLogado carrinho

-- Controller que aguarda comandos do administrador
admController :: [Produto] -> [Cliente] -> Int -> IO ()
admController produtos clientes idProduto = do
  putStrLn $
    "=====================================\n" ++
    "     Menu de Administrador           \n" ++
    "=====================================\n\n" ++
    "  (01) Visualizar Produtos\n" ++
    "  (02) Adicionar Novo Produto\n" ++
    "  (03) Atualizar Produto por Completo\n" ++
    "  (04) Visualizar Produto por Código\n" ++
    "  (05) Visualizar Produtos por Categoria\n" ++
    "  (06) Remover Produto por Código\n" ++
    "  (07) Ler Cliente por CPF\n" ++
    "  (08) Atualizar Cliente Completo\n" ++
    "  (09) Deletar Cliente por CPF\n" ++
    "  (10) Visualizar Dashboard\n" ++
    "  (11) Sair do Modo Administrador\n" ++
    "  (12) Sair do Sistema\n"

  putStrLn "Digite a opção desejada: "
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
      exibirDashboards produtos clientes idProduto

    "11" -> do
      initialController produtos clientes idProduto

    "12" -> putStrLn "Saindo do sistema."

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      admController produtos clientes idProduto

-- Função para exibir os dashboards
exibirDashboards :: [Produto] -> [Cliente] -> Int -> IO ()
exibirDashboards produtos clientes idProduto = do
  putStrLn $ "Escolha um dashboard para exibir:\n" ++
          "01. Total de Clientes\n" ++
          "02. Clientes Mais Ativos\n" ++
          "03. Média de Compras por Cliente\n" ++
          "04. Quantidade de produtos em estoque \n" ++
          "05. Produtos com estoque baixo \n" ++
          "06. Produtos mais populares \n" ++
          "07. Total de receita gerada por produto \n" ++
          "08. Total de receita gerada por categoria \n" ++
          "09. Voltar\n"

  opcao <- getLine

  case opcao of
    "01" -> do
      putStrLn "Dashboard: Total de Clientes:"
      putStrLn "-------------"
      -- Exiba o dashboard de Total de Clientes
      let totalClientes = length clientes
      putStrLn $ "Total de Clientes: " ++ show totalClientes
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto
{-
    "02" -> do
      putStrLn "Dashboard: Clientes Mais Ativos:"
      putStrLn "-------------"
      -- Exiba o dashboard de Clientes Mais Ativos
      let clientesAtivos = clientesMaisAtivos clientes
      putStrLn "Clientes Mais Ativos:"
      mapM_ (\cliente -> putStrLn $ "  - " ++ nomeCompleto cliente ++ ": " ++ show (length (historicoCompras cliente)) ++ " compras") clientesAtivos
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "03" -> do
      putStrLn "Dashboard: Média de Compras por Cliente:"
      putStrLn "-------------"
      -- Exiba o dashboard de Média de Compras por Cliente
      let mediaCompras = mediaComprasPorCliente clientes
      putStrLn $ "Média de Compras por Cliente: " ++ show mediaCompras
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto
-}
    "04" -> do
      putStrLn "Dashboard: Quantidade de produtos em estoque:"
      putStrLn "-------------"
      -- Exiba o dashboard de Quantidade de Produtos em Estoque
      let totalEstoque = quantidadeTotalEstoque produtos
      putStrLn $ "Quantidade total de produtos em estoque: " ++ show totalEstoque
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "05" -> do
      putStrLn "Dashboard: Produtos com estoque baixo:"
      putStrLn "-------------"
      let produtosBaixosEstoque = produtosComEstoqueBaixo produtos
      mapM_ (\p -> putStrLn $ "  - " ++ getNome p ++ ": " ++ show (getQuantidade p)) produtosBaixosEstoque
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "06" -> do
      {-
      putStrLn "Dashboard: Produtos Mais Populares:"
      putStrLn "-------------"
      -- Exiba o dashboard de Produtos Mais Populares
      let produtosPopulares = produtosMaisPopulares produtos
      putStrLn "Produtos Mais Populares:"
      mapM_ (\produto -> putStrLn $ "  - " ++ nome produto ++ ": " ++ show (quantidadeVendida produto) ++ " unidades vendidas") produtosPopulares
      -}
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "07" -> do
      putStrLn "Dashboard: Total de receita gerada por produto:"
      putStrLn "-------------"
      let receitaTotal = receitaTotalPorProduto produtos
      putStrLn $ "Receita total gerada por todos os produtos: R$" ++ show receitaTotal
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "08" -> do
      putStrLn "Total de receita gerada por categoria:"
      putStrLn "Digite a categoria para calcular a receita:"
      categoria <- getLine
      putStrLn "-------------"
      let receitaPorCategoria = receitaTotalPorCategoria produtos categoria
      putStrLn $ "Receita total gerada pela categoria '" ++ categoria ++ "': R$" ++ show receitaPorCategoria
      putStrLn "-------------"
      exibirDashboards produtos clientes idProduto

    "09" -> do
      putStrLn "Voltando ao menu do administrador."
      admController produtos clientes idProduto

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      exibirDashboards produtos clientes idProduto