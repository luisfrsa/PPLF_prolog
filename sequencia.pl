%casos base, se X ou Y

inarray(X,[X|_]).

inarray(X,[Z|_]):-
	inarray(X,Z).

inarray(X,[_|Zr]):-
	inarray(X,Zr).

sequencia(I,_,L):-
    inarray(I,L).

sequencia(I,F,L):-
    I<F,	
	sequencia(Inc,F,L),
	Inc is I + 1.


%sequencia(1,3,[1,2,3]).



%sequencia(X,[X|_]).
%sequencia(X,[_|Xr]):-
	%sequencia(X,Xr).



%sequencia(I,F,S).