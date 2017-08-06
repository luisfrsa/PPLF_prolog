%%%%%%%%%%%%%%%%%%%%%%%%%%%%TESTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(plunit)).

:- begin_tests(sequencia).
test(sequencia1) :- sequencia(1,1, [1]).
test(sequencia2) :- sequencia(1,2, [1,2]).
test(sequencia3) :- sequencia(1,5, [1,2,3,4,5]).
test(sequencia4) :- sequencia(2,4, [2,3,4]).
test(sequencia5, [fail]) :- sequencia(2,1, false).
test(sequencia6, [fail]) :- sequencia(5,1, false).
:- end_tests(sequencia).

:- begin_tests(permutacao).
test(permutacao1, [nondet]) :- permutacao([],[]).
test(permutacao2, [nondet]) :- permutacao([1,2],[1,2]).
test(permutacao3, [nondet]) :- permutacao([1,2],[2,1]).
test(permutacao4, [nondet]) :- permutacao([1,2,3],[2,1,3]).
test(permutacao5, [nondet]) :- permutacao([1,2,3],[3,2,1]).
test(permutacao6, [fail]) :- permutacao([1,2,3],[1,1,2,3]). 
:- end_tests(permutacao).

:- begin_tests(entre).
test(entre1, [nondet]) :- entre(1,3,1).
test(entre2, [nondet]) :- entre(1,3,2).
test(entre3, [nondet]) :- entre(1,3,3).
test(entre4, [nondet]) :- entre(1,5,5).
test(entre5, [fail]) :- entre(3,1,1).
test(entre6, [fail]) :- entre(1,3,4).
:- end_tests(entre).

:- begin_tests(not_contains).
test(not_contains1) :- not_contains(0,[4, 5, 3, 2, 1]).
test(not_contains2) :- not_contains(3,[4, 5, 1, 2, 1]).
test(not_contains3) :- not_contains(8,[4, 5, 3, 2, 1]).
test(not_contains4, [fail]) :- not_contains(1,[4, 5, 3, 2, 1]).
test(not_contains5, [fail]) :- not_contains(2,[4, 5, 3, 2, 1]).
test(not_contains6, [fail]) :- not_contains(3,[4, 5, 3, 2, 1]).
:- end_tests(not_contains).

%não verifica diagonais, apenas linhas

:- begin_tests(verificaNumIguais).
test(verificaNumIguais1) :- verificaNumIguais([1, 2, 3, 4, 5]).
test(verificaNumIguais2) :- verificaNumIguais([2, 3, 5, 7, 9]).
test(verificaNumIguais3) :- verificaNumIguais([4, 5, 3, 2, 1]).
test(verificaNumIguais4, [fail]) :- verificaNumIguais([4, 5, 1, 2, 1]).
test(verificaNumIguais5, [fail]) :- verificaNumIguais([4, 2, 3, 2, 1]).
test(verificaNumIguais6, [fail]) :- verificaNumIguais([3, 5, 3, 2, 1]).
:- end_tests(verificaNumIguais).

%não verifica linhas, apenas diagonais
:- begin_tests(validaDiagonalCrescenteElemento).
test(validaDiagonalCrescenteElemento1) :- validaDiagonalCrescenteElemento(1,[3, 5, 7, 9]). 
test(validaDiagonalCrescenteElemento2) :- validaDiagonalCrescenteElemento(3,[5, 2, 4, 8]).
test(validaDiagonalCrescenteElemento3) :- validaDiagonalCrescenteElemento(2,[4, 1, 6, 8]).
test(validaDiagonalCrescenteElemento4, [fail]) :- validaDiagonalCrescenteElemento(1,[2, 5, 1, 2, 1]).
test(validaDiagonalCrescenteElemento5, [fail]) :- validaDiagonalCrescenteElemento(3,[4, 4, 3, 2, 1]).
test(validaDiagonalCrescenteElemento6, [fail]) :- validaDiagonalCrescenteElemento(2,[3, 5, 3, 2, 1]).
:- end_tests(validaDiagonalCrescenteElemento).

%não verifica linhas, apenas diagonais
:- begin_tests(validaDiagonalDecrescenteElemento).
test(validaDiagonalDecrescenteElemento1) :- validaDiagonalDecrescenteElemento(7,[5, 3, 7, 9]).
test(validaDiagonalDecrescenteElemento2) :- validaDiagonalDecrescenteElemento(6,[4, 2, 4, 8]).
test(validaDiagonalDecrescenteElemento3) :- validaDiagonalDecrescenteElemento(5,[3, 1, 6, 8]).
test(validaDiagonalDecrescenteElemento4, [fail]) :- validaDiagonalDecrescenteElemento(7,[6, 5, 1, 2, 1]).
test(validaDiagonalDecrescenteElemento5, [fail]) :- validaDiagonalDecrescenteElemento(6,[5, 4, 3, 2, 1]).
test(validaDiagonalDecrescenteElemento6, [fail]) :- validaDiagonalDecrescenteElemento(5,[4, 5, 3, 2, 1]).
:- end_tests(validaDiagonalDecrescenteElemento).

:- begin_tests(verificaDiagonais).
test(verificaDiagonais1) :- verificaDiagonais([2, 4, 6, 8]).
test(verificaDiagonais2) :- verificaDiagonais([7, 5, 3, 1]).
test(verificaDiagonais3) :- verificaDiagonais([2, 4, 1, 3]).
test(verificaDiagonais4, [fail]) :- verificaDiagonais([1 ,2, 4, 3]).
test(verificaDiagonais5, [fail]) :- verificaDiagonais([3, 2, 4, 1]).
test(verificaDiagonais6, [fail]) :- verificaDiagonais([2, 4, 5, 8]).
:- end_tests(verificaDiagonais).

:- begin_tests(rainhas_p).
test(rainhas_p1, [nondet]) :- rainhas_p([3, 1, 4, 2], 4).
test(rainhas_p2, [nondet]) :- rainhas_p([2, 4, 1, 3], 4).
test(rainhas_p3, [nondet]) :- rainhas_p([4, 2, 5, 3, 1], 5).
test(rainhas_p4, [nondet]) :- rainhas_p([5, 3, 1, 4, 2], 5).
test(rainhas_p5, [nondet]) :- rainhas_p([1, 4, 2, 5, 3], 5).
test(rainhas_p6, [nondet]) :- rainhas_p([2, 4, 1, 3, 5], 5).
test(rainhas_p7, [fail]) :- rainhas_p([1, 2, 3, 4],4).
test(rainhas_p8, [fail]) :- rainhas_p([1, 4, 3, 2],4).
test(rainhas_p9, [fail]) :- rainhas_p([2, 4, 5, 8],4).
test(rainhas_p10, [fail]) :- rainhas_p([1, 4, 1, 3, 5], 5).
test(rainhas_p11, [fail]) :- rainhas_p([2, 4, 1, 3, 2], 5).
test(rainhas_p11, [fail]) :- rainhas_p([5, 4, 1, 3, 2], 5).
:- end_tests(rainhas_p).


:- begin_tests(rainhas_n).
test(rainhas_n1, [nondet]) :- rainhas_n([3, 1, 4, 2], 4).
test(rainhas_n2, [nondet]) :- rainhas_n([2, 4, 1, 3], 4).
test(rainhas_n3, [nondet]) :- rainhas_n([4, 2, 5, 3, 1], 5).
test(rainhas_n4, [nondet]) :- rainhas_n([5, 3, 1, 4, 2], 5).
test(rainhas_n5, [nondet]) :- rainhas_n([1, 4, 2, 5, 3], 5).
test(rainhas_n6, [nondet]) :- rainhas_n([2, 4, 1, 3, 5], 5).
test(rainhas_n7, [fail]) :- rainhas_n([1, 2, 3, 4],4).
test(rainhas_n8, [fail]) :- rainhas_n([1, 4, 3, 2],4).
test(rainhas_n9, [fail]) :- rainhas_n([2, 4, 5, 8],4).
test(rainhas_n10, [fail]) :- rainhas_n([1, 4, 1, 3, 5], 5).
test(rainhas_n11, [fail]) :- rainhas_n([2, 4, 1, 3, 2], 5).
test(rainhas_n11, [fail]) :- rainhas_n([5, 4, 1, 3, 2], 5).
:- end_tests(rainhas_n).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%TESTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------- VERSÃO GERAR E TESTAR COM PERMUTAÇÕES -------------------

%% rainhas_p(?Q, +N) is nondet.
%  Verdadeiro se Q é uma solução de tamanho N com N rainhas.
%  Este predicado constrói as possíves soluções do N-rainhas.
rainhas_p(Q, N) :-
	sequencia(1, N, R),
	permutacao(R, Q),
	solucao(Q).


%% sequencia(+I, +F, ?S) is semidet.
%  Verdadeiro se S é uma lista com os números inteiros entre I e F (inclusive)

sequencia(X,X,[X]):-!.

sequencia(I,F,[I|Sr]):-
	I < F,
	X0 is I + 1,
	sequencia(X0,F,Sr).


%% permutacao(?L, ?P) is nondet.
%  Verdadeiro se P é uma permutação da lista L

permutacao([], []).

permutacao([X], [X]) :-!.

permutacao([Xc|Xr], Y) :- 
	permutacao(Xr, H1),
	append(R1, R2, H1),
	append(R1, [Xc], X1),
	append(X1, R2, Y).


%% -------------------------- VERSÃO COM BACKTRACKING --------------------------

% rainhas_n(?Q, +N) is nondet.
% predicado "interface" para o rainhas/3
rainhas_n(Q, N) :-
	rainhas_n(Q, N, N).


%% rainhas_n(?Q, +N, +T) is nondet.
%  Verdadeiro se Q é uma solução de tamanho T com N rainhas.
%  Este é um predicado auxiliar e não deve ser chamado diretamente.
%  Sua prova deve ser realizada com N = T.
%  Exemplo:
%    ?- rainhas_n(Q, 5, 5).
%    Q = [4, 2, 5, 3, 1] ;
%    Q = [3, 5, 2, 4, 1] ;
%    Q = [5, 3, 1, 4, 2] ;
%    ...
rainhas_n([R|Rs], N, T) :-
	T > 0, !,
	T0 is T-1,
	rainhas_n(Rs, N, T0),
	entre(1, N, R),
	solucao([R|Rs]).

rainhas_n([], _, 0).


%% entre(+I, +F, ?V) is nondet.
%  Verdadeiro se V é um número natural entre I e F (inclusive).
%  Exemplo:
%    ?- entre(1, 3, V).
%    V = 1;
%    V = 2;
%    V = 3;
%    false.

entre(X,_,X).

entre(I,F,R):-
	I < F, 
	Is is I + 1,
	entre(Is,F,R).


%% solucao(+Q) is semidet.
%  Verdadeiro se Q é uma solução N-rainhas
%  Este predicado apenas verifica se Q é uma solução, e não a constrói.
%  Exemplo:
%    ?- solucao([4, 5, 3, 2, 1]).
%    true.
solucao(Q) :-
	verificaNumIguais(Q),
	verificaDiagonais(Q).


%% not_contains(E,L).
%  Verdadeiro se E não esta presente em L
%  Exemplo:
%    ?- not_contains(0,[4, 5, 3, 2, 1]).
%    true.
%    ?- not_contains(1,[4, 5, 3, 2, 1]).
%    false.
not_contains(_,[]):-!.
not_contains(E,[Lc|Lr]):-
	E \= Lc,
	not_contains(E,Lr).


%% verificaNumIguais(L).
%  Verdadeiro se L não apresenta elementos repetidos
%  Exemplo:
%    ?- verificaNumIguais([4, 5, 3, 2, 1]).
%    true.
%    ?- verificaNumIguais([4, 2, 3, 2, 1]).
%    false.
verificaNumIguais([]).

verificaNumIguais([Lc|Lr]):-
	not_contains(Lc,Lr), 
	verificaNumIguais(Lr).


%% validaDiagonalCrescenteElemento(E,L).
%  Verdadeiro se L não apresenta elementos em suas diagonais Crescentes
%  Exemplo:
%    ?- validaDiagonalCrescenteElemento(1,[3,...]).
%    true.
%    ?- validaDiagonalCrescenteElemento(1,[2,...]).
%    false.
validaDiagonalCrescenteElemento(_,[]):-!.
validaDiagonalCrescenteElemento(E,[Lc|Lr]):- 
	E0 is E + 1,
	E0 \= Lc,
	validaDiagonalCrescenteElemento(E0,Lr).

%% validaDiagonalDecrescenteElemento(E,L).
%  Verdadeiro se L não apresenta elementos em suas diagonais Descrescentes
%  Exemplo:
%    ?- validaDiagonalDecrescenteElemento(3,[5,...]).
%    true.
%    ?- validaDiagonalDecrescenteElemento(3,[2,...]).
%    false.
validaDiagonalDecrescenteElemento(_,[]):-!.
validaDiagonalDecrescenteElemento(E,[Lc|Lr]):-
	E0 is E - 1,
	E0 \= Lc,
	validaDiagonalDecrescenteElemento(E0,Lr).

%% verificaDiagonais(L).
%  Verdadeiro se L não apresenta elementos em suas diagonais Crescentese Descrescentes
%  Exemplo:
%    ?- verificaDiagonais([2,4,1,3...]).
%    true.
%    ?- verificaDiagonais([2,3,5,4,...]).
%    false.
verificaDiagonais([]).
verificaDiagonais([Lc|Lr]):-
	validaDiagonalCrescenteElemento(Lc,Lr),
	validaDiagonalDecrescenteElemento(Lc,Lr),
	verificaDiagonais(Lr).