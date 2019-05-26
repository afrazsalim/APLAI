:- lib(ic).
:- compile(helper_functions).


solve(P) :-
	read_puzzle(P,S),
	convert_to_array(R,S),
	impose_constraint(R),
	search(R,0,first_fail,indomain,complete,[]),
	writeSudoku(R, P).

impose_constraint(R) :-
	dim(R,[N,N]),
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
	).


writeSudoku(S, Name) :-
	writeln("SOLUTION OF :"),
	writeln(Name),
	(foreacharg(R, S)
	do
		array_list(R, L),
		writeln(L)
	)
	.
