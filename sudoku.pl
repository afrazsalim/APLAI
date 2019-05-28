:- compile(helper_functions).
:- compile(eclipse_constraints).

solve(P) :-
	read_puzzle(P,S),
	convert_to_array(R,S),
	impose_constraint(R),%See the eclipse_constraints file.
	search(R,0,first_fail,indomain,complete,[]),
	writeSudoku(R, P).



writeSudoku(S, Name) :-
	writeln("SOLUTION OF :"),
	writeln(Name),
	(foreacharg(R, S)
	do
		array_list(R, L),
		writeln(L)
	).
