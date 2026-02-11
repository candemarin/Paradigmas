
% Ejercicio 1
matriz(F, C, M) :- length(M, F), maplist(flipLength(C), M).

%! flipLength(?N, ?L)
flipLength(N, L) :- length(L, N).

% Ejercicio 2
replicar(X, N, L) :- length(L, N), maplist(igualar(X), L).
%! igualar(+X, -Y).
igualar(X, Y) :- Y = X.

% Ejercicio 3
transponer([[]|_], []).
transponer(M, [H|T]) :-
    M \= [],
    maplist(sacarCabeza, M, H, R),
    transponer(R, T).

%! sacarCabeza(+L, -H, -T)
sacarCabeza([H|T], H, T).

% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).

% Ejercicio 4
pintadasValidas(r([], L)) :- length(L, S), replicar(o, S, L). % si no hay restricciones todas deber ser blancas.
pintadasValidas(r(R, L)) :-
    length(R, NR), NR > 0,
    blancosValidos(R, NR, L, B),
    intercalar(B, R, C), % C es la lista numerica intercalada [b0, r1, b1, r2, b2, . . . , rk, bk]
    pintar(C, L).

%! blancosValidos(+R, +NR, +L, -B)
% calcula la cantidad de blancos que se van a necesitar en la fila o columna y los reparte en las posibles posiciones donde se pueden ubicar
blancosValidos(R, NR, L, B) :-
    sum_list(R, Rsum),
    length(L, N),
    S is N-Rsum, % cantidad total blancos a distribuir. (total - negros)
    Sp is NR-1, % minima cantidad de separadores blancos entre restricciones. (cantidad restricciones - 1)
    K is Sp+2, % cantidad de espacios donde se pueden distribuir blancos (separadores + principio + fin).
    distribucionesBlancas(S, K, B),
    sinCerosEnMedio(B). % solo nos interesan las que tengan valores blancos en el medio porque es necesario separar restricciones

%! distribucionesBlancas(+S, +K, -L)
% L es la lista con todas las formas de sumar S con K espacios.
distribucionesBlancas(0, 0, []).
distribucionesBlancas(S, K, [H|L]) :- K >= 0, between(0, S, H), S2 is S - H, K2 is K - 1, distribucionesBlancas(S2, K2, L). 

%! pintar(+XS, -LP)
% genera la lista con simbolos [x, o] dada la lista de repeticiones intercalada.
pintar([B, N | T], LP) :-
    replicar(o, B, LB), replicar(x, N, LN),
    pintar(T, LPT),
    append(LB, LN, L), append(L, LPT, LP).
pintar([B | []], LB) :- replicar(o, B, LB).
pintar([], []).

%! sinCerosEnMedio(+L)
% L es una lista que no tiene ceros en el medio:
% sinCerosEnMedio([a1, a2, ..., a_n]) es true siempre que a2, ..., a_(n-1) \= 0.
sinCerosEnMedio(L) :- length(L, K), Kf is K - 1, not((between(2, Kf, _i), nth1(_i, L, 0))).

%! intercalar(+XS, +YS, -ZS)
% Intercala la lista XS con YS, arrancando por XS. 
intercalar([], [], []).
intercalar([X|XS], [], [X|ZS]) :- intercalar(XS, [], ZS).
intercalar([], [Y|YS], [Y|ZS]) :- intercalar([], YS, ZS).
intercalar([X|XS], [Y|YS], [X,Y|ZS]) :- intercalar(XS, YS, ZS).

% Ejercicio 5
resolverNaive(NN) :- 
    recorrerResolviendoCeldas(NN, 1, 1).

%! recorrerResolviendoCeldas(+NN, +I, +J)
% Recorre la matriz del nonograma, e intenta resolver la fila y columna correspondiente a esa celda.
recorrerResolviendoCeldas(nono(M, _), I, _) :- matriz(F, _, M), I > F.
recorrerResolviendoCeldas(nono(M, RS), I, J) :-
    matriz(F, C, M),
    I =< F, J =< C,
    Jn is J + 1,
    resolverCelda(nono(M, RS), I, J),
    recorrerResolviendoCeldas(nono(M, RS), I, Jn).
recorrerResolviendoCeldas(nono(M, RS), I, J) :-
    matriz(F, C, M),
    J > C, I =< F, In is I + 1,
    recorrerResolviendoCeldas(nono(M, RS), In, 1).

%! resolverCelda(+NN, +I, +J)
% Intenta resolver la celda I,J de la matriz, asignandole al elemento x u o para branchear en las dos posibilidades,
% se fija que esa eleccion sea consistente con las pintadas validas de la fila y columna que involucra esa celda.
resolverCelda(nono(M, RS), I, J) :-
    matriz(F, _, M),
    % E es el elem en la pos I, J de la matriz (instanciado o no).
    mnth1(I, J, M, E),
    member(E, [x, o]), % aca branchea (o condiciona a cual es).
    Jd is J + F, % Se suma F para el offset en la restricciones.
    nth1(I, RS, RFila),
    nth1(Jd, RS, RCol),
    pintadasValidas(RFila),
    pintadasValidas(RCol).

%! mnth1(+I, +J, +M, -E)
% true si el elemento E esta en la posicion I J de la matriz M
mnth1(I, J, M, E) :- nth1(I, M, F), nth1(J, F, E). 

% Ejercicio 6
pintarObligatorias(r(RS, L)) :-
    findall(L, pintadasValidas(r(RS, L)), C),
    transponer(C, CT),
    maplist(combinarCeldasList, CT, L).
    
% combinarCeldasList([], _). 
% aplica combinarCelda secuencialmente a una lista.
% combina todas las celdas de una lista (en este caso, de una fila o columna).
combinarCeldasList([X], X).
combinarCeldasList([A, B| T], X) :-
    combinarCelda(A,B,Xs),
    combinarCeldasList([Xs|T], X).

% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
deducir1Pasada(nono(_, RS)) :- 
    maplist(pintarObligatorias, RS).
    
% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo mas cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).

% Ejercicio 8
restriccionConMenosLibres(nono(_, RS), r(R, L)) :-
    member(r(R, L), RS), cantidadVariablesLibres(L, FV), FV > 0,
    not((member(r(_, L1), RS), cantidadVariablesLibres(L1, FV2), FV2 > 0, FV2 < FV)). % si no hay restricciones con variables libres da false.

% Ejercicio 9
resolverDeduciendo(nono(M, RS)) :-
    deducirVariasPasadas(nono(M, RS)),
    cantidadVariablesLibres(M, 0). % si no hay variables libres ya esta resuelto.

resolverDeduciendo(nono(M, RS)) :-
    deducirVariasPasadas(nono(M, RS)),
    restriccionConMenosLibres(nono(M, RS), R), !, % corte para que solo nos de una de las minimimas y no obtengamos repetidos.
    pintadasValidas(R),
    resolverDeduciendo(nono(M, RS)).

% Ejercicio 10
solucionUnica(NN) :- findall(_, resolverDeduciendo(NN), NS), length(NS, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').

tam(N, (F, C)) :- nn(N, nono(M, _)), matriz(F, C, M).
