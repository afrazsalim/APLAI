

convert_to_array(Puzzle,Temp) :-
   listToArray(Temp,Lists),
   array_list(Puzzle,Lists).



 listToArray([],[]).
 listToArray([A|As],[B|Bs]) :-
       array_list(B,A),
       listToArray(As,Bs).




print_result(Result) :-
       (
       foreachelem(E,Result,[_,J])
          do
         (J =:= 1 -> nl; true),
           write(" "),
         ( var(E) -> write("_") ; write(E))
       ).





read_puzzle(Puzzle,Model) :-
        dim(Puzzle,[N,N]),
        dim(Model,[N,N]),
        (
          multifor([I,J],[1,1],[N,N]),param(Puzzle,Model) do
              subscript(Puzzle,[I,J],Result),
              (
                var(Result) -> true
                  ;
                subscript(Model,[Result,J],I)
                  )
            ).
