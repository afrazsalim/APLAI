:- lib(ic).

:- compile("sudex_toledo").

solve(P) :- 
	puzzles(S, P),
	solution(S,R),
	writeSudoku(R, P)
	.

solution(S,R) :-
	length(S,N),
	dim(R,[N,N]),
	listToArray(S,TempS),
	array_list(R,TempS),
	R #:: 1..N,
	(for(I,1,N), param(R,N)
	do
		Rw is R[I,1..N],
		Cl is R[1..N,I],
		alldifferent(Rw),
		alldifferent(Cl)
	),
	sqrt(N,Sq),
	integer(Sq,ISq),
	(multifor([J,K],[1,1],[7,7],[ISq,ISq]), param(R)
	do
		Cube is R[J..J+2,K..K+2],
		flatten(Cube,LCube),
		alldifferent(LCube)
	),
	search(R,0,first_fail,indomain,complete,[]).

listToArray([],[]).
listToArray([A|As],[B|Bs]) :-
    array_list(B,A),
    listToArray(As,Bs).

writeSudoku(S, Name) :-
	writeln("SOLUTION OF :"),
	writeln(Name),
	(foreacharg(R, S)
	do
		array_list(R, L),
		writeln(L)
	)
	.