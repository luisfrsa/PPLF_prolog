
sequencia(X,X,[X]).

sequencia(X,Y,[X|Lr]):-
	X < Y,
	X0 is X + 1,
	sequencia(X0,Y,Lr).

%Casos:

%sequencia(1,1,[1]).
%sequencia(1,2,[1,2]).
%sequencia(1,3,[1,2,3]).
%sequencia(1,4,[1,2,3,4]).
%sequencia(1,5,[1,2,3,4,5]).
