:- compile("sudex_toledo").

read_puzzle(Name,Result) :-
  puzzles(Result,Name).


convert_to_array(Puzzle,Temp) :-
  listToArray(Temp,Lists),
  array_list(Puzzle,Lists).




listToArray([],[]).
listToArray([A|As],[B|Bs]) :-
      array_list(B,A),
      listToArray(As,Bs).


create_var_list(Puzzle,Xco,Yco,Result) :-
   flatten(Puzzle,FirstResult),
   flatten(Xco,SecondResult),
   flatten(Yco,ThirdResult),
   array_concat(FirstResult,SecondResult,Temp),
   array_concat(ThirdResult,Temp,Result).
