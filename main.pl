% formato de respostas:
% resposta(IdPergunta, s/n).

% calcula_pontuacao/3
% Procura todo peso para cada característica que recebeu "s",
% coloca em uma lista "Pesos",
% e soma os elementos da lista para um resultado final "Pontuacao".
calcula_pontuacao(Trilha, Respostas, Pontuacao) :-
    findall(Peso, 					 
            (member(Caracteristica, Respostas), perfil(Trilha, Caracteristica, Peso)),
             Pesos),				 
    sum_list(Pesos, Pontuacao). 	 


% recomenda/2
% Monta uma lista "Respostas" com todas as características onde o usuário respondeu "s", 
% roda calcula_pontuacao com a lista de respostas,
% apenas recomenda a trilha se a pontuação for maior ou igual a 7.
recomenda(Trilha, Pontuacao) :-
    trilha(Trilha, _),
	findall(Caracteristica,
        (resposta(Id, s),
         pergunta(Id, _, Caracteristica)),
        Respostas),
    calcula_pontuacao(Trilha, Respostas, Pontuacao),
    Pontuacao >= 7.

% ordena_recomendacoes/1
% Cria uma lista "Resultados" de pares compostos por pontuação e trilha
% Exemplo: [5-inteligencia_artificial]
% Ordena a lista pelo primeiro termo
% Argumentos do sort:
% 0 -> não remover itens duplicados
% @>= -> ordenar em ordem decrescente
% Resultados -> a lista a ser ordenada
% TrilhasOrdenadas -> lista resultante
ordena_recomendacoes(TrilhasOrdenadas) :-
    findall(Pontuacao-Trilha,
            recomenda(Trilha, Pontuacao),
            Resultados),
	sort(0, @>=, Resultados, TrilhasOrdenadas).


% O predicado resposta tem que ser dynamic 
% pra poder armazenar as respostas do usuário através do assertz.
:- dynamic resposta/2.

% Aqui o programa é iniciado, o usuário tem que chamar o "iniciar".
iniciar :-
    writeln('Sistema Especialista para Recomendação
de Trilha Acadêmica'),
    faz_perguntas.


% No forall vai ser criado um laço de repetição com o repeat.
% Vai ser impresso "(s/n)" e o texto, que é a pergunta.
% "read" lê o que o usuario digitou e guarda como "Resp".
% "!" serve para impedir que o repeat rode de novo caso "s" ou "n" seja verdadeiro,
% então vai pro assertz.
% Se não for "s" ou "n" o "!" impede que vá para o assertz e o repeat roda novamente.
% O assertz adiciona a resposta no predicdo resposta.
faz_perguntas :-
    writeln("Responda com 's' para 'sim' e 'n' para 'não'."),
    forall(pergunta(Id, Texto, _),
           (
               repeat,
                 format("~w ~n(s/n): ", [Texto]),
                 read(Resp),
                 (Resp == s ; Resp == n), !,
                 assertz(resposta(Id, Resp))
           )),
    ordena_recomendacoes(Trilhas),
    writeln("Recomendações:"),
    exibe_resultado(Trilhas).


% Pega o primeiro resultado das trilhas e separa entre pontuação e trilha pro head,
% os outros resultados ficam como resto no tail,
% repete isso até a lista ficar vazia.
exibe_resultado([]).
exibe_resultado([Pont-Trilha | Resto]) :-
    format("Trilha: ~w - Pontos: ~w~n", [Trilha, Pont]),
    exibe_resultado(Resto).



% ---------------------------------------------------------
% Modo teste: carrega respostas de um arquivo, como solicitado no documento
% (Só funciona se tiver como ler de arquivos separados - não funciona no SWISH)
teste(Arquivo) :-
    writeln('Sistema Especialista para Recomendação de Trilha Acadêmica'),
    writeln('Modo teste'),
    consulta_arquivo(Arquivo).

% retractall apaga respostas anteriores pra evitar conflitos
% consult carrega o arquivo especificado e afirma todos os predicados do arquivo
% Depois, a lógica é igual a em faz_perguntas
consulta_arquivo(Arquivo) :-
    retractall(resposta(_, _)),
    consult(Arquivo),
    ordena_recomendacoes(Trilhas),
    writeln("Recomendações:"),
    exibe_resultado(Trilhas).

% para rodar:
% ?- teste('perfil_teste_1.pl').



% -----------------------------------------------------------
% Modo teste local
% Carrega dados diretamente pelo mesmo arquivo

% Base de dados:

% Perfil 1: Inteligência Artificial
perfil_teste(1, [
    resposta(1,s), resposta(2,s), resposta(3,s), resposta(4,s), resposta(5,s),
    resposta(6,n), resposta(7,n), resposta(8,n), resposta(9,n), resposta(10,n),
    resposta(11,n), resposta(12,n), resposta(13,n), resposta(14,n), resposta(15,n),
    resposta(16,n), resposta(17,n)
]).

% Perfil 2: Desenvolvimento Web
perfil_teste(2, [
    resposta(1,n), resposta(2,n), resposta(3,n), resposta(4,n), resposta(5,n),
    resposta(6,s), resposta(7,s), resposta(8,s), resposta(9,s), resposta(10,s),
    resposta(11,n), resposta(12,n), resposta(13,n), resposta(14,n), resposta(15,n),
    resposta(16,n), resposta(17,n)
]).

% Perfil 3: Cibersegurança
perfil_teste(3, [
    resposta(1,s), resposta(2,n), resposta(3,s), resposta(4,n), resposta(5,n),
    resposta(6,n), resposta(7,n), resposta(8,n), resposta(9,s), resposta(10,n),
    resposta(11,s), resposta(12,s), resposta(13,n), resposta(14,n), resposta(15,n),
    resposta(16,n), resposta(17,n)
]).

% Perfil 4: Redes e Infraestrutura
perfil_teste(4, [
    resposta(1,n), resposta(2,n), resposta(3,n), resposta(4,n), resposta(5,n),
    resposta(6,n), resposta(7,n), resposta(8,n), resposta(9,s), resposta(10,n),
    resposta(11,s), resposta(12,s), resposta(13,n), resposta(14,s), resposta(15,s),
    resposta(16,n), resposta(17,n)
]).

% Perfil 5: Pesquisador Científico
perfil_teste(5, [
    resposta(1,n), resposta(2,n), resposta(3,n), resposta(4,s), resposta(5,n),
    resposta(6,n), resposta(7,n), resposta(8,n), resposta(9,s), resposta(10,n),
    resposta(11,n), resposta(12,n), resposta(13,n), resposta(14,s), resposta(15,n),
    resposta(16,s), resposta(17,s)
]).


% Função principal:
teste_local(Id) :-
    writeln('Sistema Especialista - Modo teste'),
    retractall(resposta(_,_)),
    perfil_teste(Id, Respostas),
    forall(member(resposta(Id_R, Resposta), Respostas),
           assertz(resposta(Id_R, Resposta))),
    ordena_recomendacoes(Trilhas),
    writeln("Recomendações:"),
    exibe_resultado(Trilhas).

% para rodar:
% ?- teste_local(1).
