:- compile(eclipse_constraints).

solve(PuzzleName) :-
  read_puzzle(PuzzleName,Temp),
  convert_to_array(Puzzle,Temp),
  outer_call_alt(Puzzle,Model),
  outer_call_simple(Puzzle),
  channeling_constraint(Puzzle,Model),
  create_Variable_list(Puzzle,Model,Result),
  search(Result,0,first_fail,indomain,complete,[]),
  print_sudoku(Puzzle).



create_Variable_list(Puzzle,Model,Result) :-
     array_flat(1,Puzzle,FR),
     array_flat(1,Model,SR),
     term_variables(FR,V1),
     term_variables(SR,V2),
     append(V1,V2,Result).


channeling_constraint(Model,AltModel) :-
   dim(Model,[N,N]),
  (
  multifor([I,J] ,[1,1],[N,N]),param(Model,AltModel) do
      E #:: 1..9,
      Model[I,J] #= E,
      element(E,AltModel[I],J)
  ).


print_sudoku(Result) :-
  dim(Result,[N,N]),
  (
   multifor([I,J],[1,1],[N,N]),param(Result,N) do
     subscript(Result,[I,J],Number),
     write(" "),
     write(Number),
     (
      J =:= N -> writeln(" ")
      ;
      true
     )
  ).
