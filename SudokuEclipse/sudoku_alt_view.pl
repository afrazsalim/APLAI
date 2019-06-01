:- lib(ic).
:- lib(ic_global).
:- import alldifferent/1 from ic_global.
:- compile(sudex_toledo).

solve(Name) :-
   puzzles(Puzzle,Name),
   convert_to_array(Temp,Puzzle),
   read_puzzle(Temp,Model),
   impose_constraint_alt(Model),
   search_board(Model),
   convert_model(Model,Result),
   print_result(Result).



search_board(Result) :-
   term_variables(Result,Vars),
   labeling(Vars).






convert_model(Model,Result) :-
   dim(Model,[N,N]),
   dim(Result,[N,N]),
   (
    multifor([I,J],[1,1],[N,N]),param(Model,Result) do
    subscript(Model,[I,J],Data),
    subscript(Result,[Data,J],I)
   ).
