
% Exemplo 3.4

%% membro(?X, ?XS) is nondet
%
%  Verdadeiro se X é um elemento de XS.
%
%  Veja o predicado pré-definido member/2.

/*:- begin_tests(membro).

test(t0, [nondet]) :- membro(1, [1, 3, 7]).
test(t1, [nondet]) :- membro(3, [1, 3, 7]).
test(t2, [nondet]) :- membro(7, [1, 3, 7]).
test(t3, all(X == [1, 3, 7, -2])) :- membro(X, [1, 3, 7, -2]).

:- end_tests(membro).*/
%casos:
%[1]->[1],
%[1,2]->[1,2],[2,1]
%[1,2,3]->[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]

%permutacao([X | _], [X|_]).
%permutacao([X | _], [_|X]).
%permutacao([_|X], [_|X]).
%permutacao([_|X], [X|_]).

%permutacao(X,[X|_]).
%
%membro(X,[_|YS]):-
%	membro(X,YS). 

membro(X,[X|_]).
membro(X,[_|YS]):-
	membro(X,YS). 

permutacao([],_):-!.

permutacao([X|Xr],Y):-
	membro(X,Y),
	permutacao(Xr,Y).

%permutacao([X|Xr],[X|Yr]):-
%	permutacao(Xr,Yr).


ver 
inverte(X,X).

inverte([Xc|Xr],Li):-
	inverte(Xr,L0),
	Li = [L0,Xc].


palindromo(L).




%.
%
%permutacao(X,Y).
%
%membro(X, [X | _]).
%membro(X, [_ | XS]) :-
%    membro(X, XS).
%
% 
%
%permutacao(X, [X | _]).
%permutacao(X, [_ | XS]) :-
%    permutacao(X, XS).
%
%permutacao([X | Xr], YL) :-
%    permutacao(X, YL),
%    permutacao(Xr, YL).
%


