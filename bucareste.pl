%%%%%%%%%%%%%%%%%%%%%%%%%%%%TESTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(plunit)).
  
:- begin_tests(calculasomaheuristica).
test(calculasomaheuristica1) :- calcula_soma_heuristica(rimnicu_vilcea,80, 273).
test(calculasomaheuristica2) :- calcula_soma_heuristica(urziceni,98, 178).
test(calculasomaheuristica3) :- calcula_soma_heuristica(timisoara, 111,440).
test(calculasomaheuristica4) :- calcula_soma_heuristica(craiova,120,280 ).
test(calculasomaheuristica5) :- calcula_soma_heuristica(mehadia, 70,311).
test(calculasomaheuristica6) :- calcula_soma_heuristica(arad,140,506).
:- end_tests(calculasomaheuristica).

:- begin_tests(getmelhorvizinho).
test(getmelhorvizinho1) :- get_melhor_vizinho([d(mehadia,75), d(craiova,120)], 120, craiova).
test(getmelhorvizinho2) :- get_melhor_vizinho([d(eforie,86), d(urziceni, 98)],98, urziceni).
test(getmelhorvizinho3) :- get_melhor_vizinho([d(iasi, 92), d(urziceni, 142)], 142,urziceni).
test(getmelhorvizinho4) :- get_melhor_vizinho([d(hirsova,86)],86,hirsova ).
test(getmelhorvizinho5) :- get_melhor_vizinho([d(zerind, 71), d(sibiu, 151)], 151,sibiu).
test(getmelhorvizinho6) :- get_melhor_vizinho([d(iasi, 92), d(urziceni, 142)],142,urziceni).
:- end_tests(getmelhorvizinho).
 
:- begin_tests(melhorvizinho).
test(melhorvizinho1) :- melhor_vizinho(hirsova,urziceni ).
test(melhorvizinho2) :- melhor_vizinho(urziceni,bucharest ).
test(melhorvizinho3) :- melhor_vizinho(oradea, sibiu).
test(melhorvizinho4) :- melhor_vizinho(vaslui,urziceni ).
test(melhorvizinho5) :- melhor_vizinho(pitesti, bucharest). 
test(melhorvizinho6) :- melhor_vizinho(neamt,iasi).
:- end_tests(melhorvizinho).

:- begin_tests(melhorcaminho).
test(melhorcaminho1) :- melhor_caminho(sibiu, [sibiu, rimnicu_vilcea, pitesti, bucharest]).
test(melhorcaminho2) :- melhor_caminho(eforie,[eforie, hirsova, urziceni, bucharest] ).
test(melhorcaminho3) :- melhor_caminho(oradea,[oradea, sibiu, rimnicu_vilcea, pitesti, bucharest] ).
test(melhorcaminho4) :- melhor_caminho(craiova, [craiova, pitesti, bucharest]).
test(melhorcaminho5) :- melhor_caminho(pitesti, [pitesti, bucharest]).
test(melhorcaminho6) :- melhor_caminho(neamt,[neamt, iasi, vaslui, urziceni, bucharest] ).
:- end_tests(melhorcaminho).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%TESTES%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vizinhos(arad, [d(sibiu,140),d(timisoara,118), d(zerind,75)]).
vizinhos(bucharest, [d(fagaras,211), d(giurgiu,90), d(pitesti,101), d(urziceni,85)]).
vizinhos(craiova, [d(dobreta,120), d(pitesti,138), d(rimnicu_vilcea,146)]).
vizinhos(dobreta, [d(mehadia,75), d(craiova,120)]).
vizinhos(eforie, [d(hirsova,86)] ).
vizinhos(fagaras, [d(sibiu,99), d(bucharest,211)] ).
vizinhos(giurgiu, [d(bucharest,90)]).
vizinhos(hirsova, [d(eforie,86), d(urziceni, 98)] ).
vizinhos(iasi, [d(vaslui, 92),d(neamt, 87)]).
vizinhos(lugoj, [d(mehadia, 70), d(timisoara, 111)]).
vizinhos(mehadia, [d(dobreta, 75), d(lugoj, 70)]).
vizinhos(neamt, [d(iasi, 87)]).
vizinhos(oradea, [d(zerind, 71), d(sibiu, 151)]).
vizinhos(pitesti, [d(rimnicu_vilcea, 97), d(craiova, 138), d(bucharest, 101)]). 
vizinhos(rimnicu_vilcea, [d(sibiu, 80), d(craiova, 146), d(pitesti, 97)]).
vizinhos(sibiu, [d(arad, 140), d(oradea, 151), d(fagaras, 99), d(rimnicu_vilcea, 80)]).
vizinhos(timisoara, [d(arad, 118), d(lugoj, 111)]).
vizinhos(urziceni, [d(bucharest, 85), d(hirsova, 98), d(vaslui, 142)]).
vizinhos(vaslui, [d(iasi, 92), d(urziceni, 142)]).
vizinhos(zerind, [d(arad, 75), d(oradea, 71)]).


heuristica(arad, 366).
heuristica(bucharest, 0).
heuristica(craiova, 160).
heuristica(dobreta, 238).
heuristica(eforie, 161).
heuristica(fagaras, 178).
heuristica(giurgiu, 77).
heuristica(hirsova, 151).
heuristica(iasi, 226).
heuristica(lugoj, 244).
heuristica(mehadia, 241).
heuristica(neamt, 234).
heuristica(oradea, 380).
heuristica(pitesti, 98).
heuristica(rimnicu_vilcea, 193).
heuristica(sibiu, 253).
heuristica(timisoara, 329).
heuristica(urziceni, 80).
heuristica(vaslui, 199).
heuristica(zerind, 374).



%% melhor_caminho(?O, ?C) is nondet
%  Verdadeiro se C é o melhor caminho entre O e bucharest
%  Exemplo:
%    ?- melhor_caminho(sibiu, C).
%    C = [sibiu, rimnicu_vilcea, pitesti, bucharest].
melhor_caminho(bucharest, [bucharest]) :- !.
melhor_caminho(O, [O|Cs]) :-
	melhor_vizinho(O, V), !,
	melhor_caminho(V, Cs).
 

%% melhor_vizinho(?O, ?V) is nondet
%  Verdadeiro se V é o melhor vizinho de O
%  Exemplo:
%    ?- melhor_vizinho(sibiu, V).
%    V = rimnicu_vilcea.
melhor_vizinho(C, V) :-
	vizinhos(C,ListaVizinhos),
    get_melhor_vizinho(ListaVizinhos,_,V),
	%get_melhor_vizinho(ListaVizinhos,MelhorVizinho,V), caso precise usar o valor [(MelhorVizinho)]
    !.


%% get_melhor_vizinho(ListaVizinhos, MelhorVizinhoAtual,MenorDistancia,MelhorVizinho) 
%  Verdadeiro se MenorDistancia é a distancia do vizinho, este vizinho é o vizinho cujo sua distancia, 
%  somada com sua heurística, é a menor dentre os vizinhos
%  E verdadeiro se MelhorVizinho é o vizinho cujo sua distancia, 
%  somada com sua heurística, é a menor dentre os vizinhos
%  Exemplo:
%    ?- get_melhor_vizinho([d(arad, 140), d(oradea, 151), d(fagaras, 99), d(rimnicu_vilcea, 80)], MelhorVizinhoAtual,MenorDistancia,MelhorVizinho).
%    MenorDistancia = 80.
%    MelhorVizinho = rimnicu_vilcea.
get_melhor_vizinho([Vizinho|ListaVizinhos],MenorValor,MelhorVizinhoFinal):-
    get_melhor_vizinho(ListaVizinhos,Vizinho,MenorValor,MelhorVizinhoFinal).

get_melhor_vizinho([],MenorVizinho,MenorValor,MelhorVizinhoFinal):-
    MenorVizinho = d(V,D),
    MenorValor = D,
    MelhorVizinhoFinal = V.

get_melhor_vizinho([Vizinho_c|ListaVizinho_r],MelhorVizinho,MenorValor,MelhorVizinhoFinal):-
    Vizinho_c = d(CidadeVizinho,DistanciaCidade),
    MelhorVizinho = d(CidadeMelhorVizinho,DistanciaMelhor),

    calcula_soma_heuristica(CidadeVizinho,DistanciaCidade,SomaDistHeuCidade),
    calcula_soma_heuristica(CidadeMelhorVizinho,DistanciaMelhor,SomaDistHeuMelhor),

    SomaDistHeuMelhor < SomaDistHeuCidade,

    get_melhor_vizinho(ListaVizinho_r, MelhorVizinho, MenorValor,MelhorVizinhoFinal).

   
get_melhor_vizinho([Vizinho_c|ListaVizinho_r],MelhorVizinho,MenorValor,MelhorVizinhoFinal):-
    Vizinho_c = d(CidadeVizinho,DistanciaCidade),
    MelhorVizinho = d(CidadeMelhorVizinho,DistanciaMelhor),

    calcula_soma_heuristica(CidadeVizinho,DistanciaCidade,SomaDistHeuCidade),
    calcula_soma_heuristica(CidadeMelhorVizinho,DistanciaMelhor,SomaDistHeuMelhor),

    SomaDistHeuMelhor >= SomaDistHeuCidade,

    get_melhor_vizinho(ListaVizinho_r, Vizinho_c, MenorValor,MelhorVizinhoFinal).


%% calcula_soma_heuristica(Cidade,Distancia,Resultado) 
%  Verdadeiro se Resultado é a soma da Distancia da Cidade, com sua heuristica
%  Exemplo:
%    ?- calcula_soma_heuristica(rimnicu_vilcea,80, Resultado).
%    Resultado = 193 + 80.

calcula_soma_heuristica(Cidade,Distancia,Resultado):-
    heuristica(Cidade,DistanciaHeuristica),
    Resultado is Distancia + DistanciaHeuristica.



