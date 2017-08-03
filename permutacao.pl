
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

membro(X, [X | _]).
membro(X, [_ | XS]) :-
    membro(X, XS).

 

permutacao(X, [X | _]).
permutacao(X, [_ | XS]) :-
    permutacao(X, XS).

permutacao([X | Xr], YL) :-
    permutacao(X, YL),
    permutacao(Xr, YL).



