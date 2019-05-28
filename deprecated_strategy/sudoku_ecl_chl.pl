
:- compile("Sudoku_alt_view_dep").
:- compile("sudoku").


solve_with_chan(Puzzle,Name) :-
   read_puzzle(Name,Result),
   convert_to_array(Puzzle,Result),
   constrain_model(Puzzle,Xco,Yco),
   impose_constraint(Puzzle),
   channeling_constraints(Puzzle,Xco,Yco),
   create_var_list(Puzzle,Xco,Yco,Variables),
   labeling(Variables),
   print_result(Puzzle).


channeling_constraints(Puzzle,Xco,Yco) :-
    dim(Puzzle,[N,N]),
   (
    multifor([I,J] ,[1,1],[N,N]),param(Puzzle,Xco,Yco) do
        E #:: 1..9,
        Puzzle[I,J] #= E,
        element(E,Yco[I],J),
        element(E,Xco[I],I)
   ).
