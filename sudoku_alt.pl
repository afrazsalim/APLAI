:- lib(ic).
:- lib(ic_global).
:- import alldifferent/1 from ic_global.
:- compile(helper_functions).
:- compile(eclipse_constraints).

solve(Name) :-
  read_puzzle(Name,Temp),
  convert_to_array(Puzzle,Temp),
  build_model(Puzzle,Model),
  constrain_model_alt(Model),
  search_board(Model),
  convert_back_to_real(Model,Result),
  print_out(Result).




print_out(Result) :-
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



convert_back_to_real(Model,Result) :-
        dim(Model,[N,N]),
        dim(Result,[N,N]),
        (
          multifor([I,J],[1,1],[N,N]),param(Model,Result) do
             subscript(Model,[I,J],Num),
             subscript(Result,[I,Num],J)
        ).





search_board(Result) :-
    term_variables(Result,Vars),
    labeling(Vars).
