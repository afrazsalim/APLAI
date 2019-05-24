
:- compile("Sudoku_alt_view").
:- compile("sudoku").


solve_with_chan(Puzzle,Name) :-
   read_puzzle(Name,Result),
   convert_to_array(Puzzle,Result),
   constrain_model(Puzzle,Xco,Yco),
   impose_constraint(Puzzle),
   channeling_constraints(Puzzle,Xco,Yco),
   create_var_list(Puzzle,Xco,Yco,Result).



channeling_constraints(Puzzle,Xco,Yco) :-
    dim(Puzzle,[N,N]),
   (
    multifor([I,J] ,[1,1],[N,N]),param(Puzzle,Xco,Yco),foreach(E,_) do
        E #:: 1..9,
        E #= Puzzle[I,J],  %Here is instantiation error. Need to fix it.
        suspend:(Yco[I,E] #= J), %Suspend is not correct to use. Need some better solution.
        suspend:(Xco[I,E] #= I)
   ).
