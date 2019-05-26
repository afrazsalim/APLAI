:- compile("sudex_toledo").

read_puzzle(Name,Result) :-
  puzzles(Result,Name).


convert_to_array(Puzzle,Temp) :-
  listToArray(Temp,Lists),
  array_list(Puzzle,Lists).




print_result(Result) :-
      (
      foreachelem(E,Result,[_,J])
      do
      (J =:= 1 -> nl; true),
      write(" "),
      ( var(E) -> write("_") ; write(E))
    ).





listToArray([],[]).
listToArray([A|As],[B|Bs]) :-
      array_list(B,A),
      listToArray(As,Bs).


create_var_list(Puzzle,Xco,Yco,Result) :-
   array_flat(1,Puzzle,FirstResult),
   array_flat(1,Xco,SecondResult),
   array_flat(1,Yco,ThirdResult),
   term_variables(FirstResult,V1),
   term_variables(SecondResult,V2),
   term_variables(ThirdResult,V3),
   append(V2,V1,Temp),
   append(V3,Temp,Result).
