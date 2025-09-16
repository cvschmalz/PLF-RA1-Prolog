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



