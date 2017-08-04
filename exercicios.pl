%%%%%%%%%%%EXERCICIOS de Pratica%%%%%%%%%%%%%%
%1) ultimo
ultimo(X,[X]).

ultimo(X,[_|Xr]):-
	ultimo(X,Xr).
%2) pares

verificaPar(0).
verificaPar(X):-
	X > 1,
	X2 is rem(X,2),
	verificaPar(X2).

verificaImpar(1).
verificaImpar(X):-
	X > 1,
	X2 is rem(X,2),
	verificaImpar(X2).
 
pares([],[]).

pares([Xc|Xr],[Xc|Yr]):-
	verificaPar(Xc),
	pares(Xr,Yr).

pares([Xc|Xr],Y):-
	verificaImpar(Xc),
	pares(Xr,Y).
	
%3) soma_lista (map)

%lista_soma([],_,_):-!.


lista_soma([],_,[]).


lista_soma([Xc|Xr],X,Y):-
	Xs is Xc + X,
	lista_soma(Xr,X,Y1),
	Y = [Xs|Y1].

%4) maximo.

max([],0):-!.


max([Xc|Xr],M):-
	max(Xr,M0),
	M is M0,
	Xc < M0.

max([Xc|Xr],M):-
	max(Xr,M0),
	M is Xc,
	Xc >= M0.

fat(0,1).

fat(N,R):-
	N > 0,
	Nm is N - 1,
	fat(Nm,R0),
	R is R0*N. 
 
%5) palindromo.

inverte([],[]).
inverte([X],[X]).

inverte([Xc|Xr],Li):-
	inverte(Xr,L0),
%	Li = [L0,Xc],
	append(L0,[Xc],Li).



pali([],[],[]).


pali([Xc|Xr],L,Li):-
	pali(Xr,L0,Li0),
	L = [Xc,L0],
	Li = [Li0,Xc].

palindromo(L):-
	inverte(L,L).

%3.11 rotaciona fail:

rot([],_,[]).

rot([Xc,Xr],C,R):-
	C>0,
	C0 is C - 1,
	rot(Xr,C0,R0),
	%append(R0,[Xc],R).
	R = [R0,Xc].


	%R = [Xc,R0].

%sub_lista

sub_lista([],_,_,_).

sub_lista(X,I,F,R):-
	sub_l(X,I,F,0,R).

sub_l([_|Xr],I,F,C,R):-
	C < I,
	Cp is C + 1,
	sub_l(Xr,I,F,Cp,R).

sub_l([Xc|Xr],I,F,C,R):-
	Fp is F + 1,
	C >= I,
	C < Fp,
	Cp is C + 1,
	sub_l(Xr,I,F,Cp,R0),
	R = [Xc|R0].


lista_soma2([],_,[]).

lista_soma2([Xc|Xr],K,R):-
	Xs is Xc + K,
	lista_soma2(Xr,K,R0),
	R = [Xs|R0].




