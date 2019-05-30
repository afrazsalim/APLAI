:- use_module(library(chr)).
:- compile("sudex_toledo").
:- compile("sort").
:- chr_type list(T) ---> [] ; [T|list(T)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint elementX(+coordinates,+values).
:- chr_constraint elementY(+coordinates,+values).
:- chr_constraint refine/0,search/1,cleanup/0,insert/0,nop/0,clean_nop/0.


solve_alt_view(Puzzle_Name) :-
     puzzles(Puzzle,Puzzle_Name),
     once(solve(Puzzle)),
     write_out(List,1),
     flatten(List,Result),
     insert_sort(Result,Temp),
     cleanup,
     print_out(Temp).
     %write_result(1,1),
     %write("Printed").



solve(Puzzle) :-
   read_xcoordinate(Puzzle,1),
   clean_nop,
   refine.

print_out([]).
print_out([P-N|T]) :-
  write(N),
  write(" "),
  (
   P mod 9 =:= 0 -> writeln(" ") ; true
  ),
  print_out(T).




write_out([],10).
write_out([H|T],X) :-
   get_result(H,X,1),
   NewX is X+1,
   write_out(T,NewX).

get_result([],_,10).
get_result([R|T],X,Y) :-
  find_chr_constraint(elementX((X,Y),[Xco])),
  find_chr_constraint(elementY((X,Y),[Yco])),
  Coordinate is (Xco-1)*9+Yco,
  R = Coordinate-Y,
  NewY is Y+1,
  get_result(T,X,NewY).



read_xcoordinate([],_).
read_xcoordinate([H|T],Xco) :-
   write_XY_to_store(H,Xco,1),
   write_const_XY_to_store(H,Xco,1),
   NewXco is Xco+1,
   read_xcoordinate(T,NewXco).

write_const_XY_to_store([],_,_).
write_const_XY_to_store([H|T],Xco,Yco) :-
      (
         var(H) ->
           NewYco is Yco+1,
           write_const_XY_to_store(T,Xco,NewYco)
         ;
           elementX((Xco,Yco),[1,2,3,4,5,6,7,8,9]),
           elementY((Xco,Yco),[1,2,3,4,5,6,7,8,9]),
           elementX((Xco,H),[Xco]),insert,
           elementY((Xco,H),[Yco]),insert,
           NewYco is Yco+1,
           write_const_XY_to_store(T,Xco,NewYco)
      ).




write_XY_to_store([],_,_).
write_XY_to_store([H|T],Xco,Yco) :-
   (
      var(H) ->
         elementX((Xco,Yco),[1,2,3,4,5,6,7,8,9]),
         elementY((Xco,Yco),[1,2,3,4,5,6,7,8,9]),
         NewYco is Yco+1,
         write_XY_to_store(T,Xco,NewYco)
      ;
        NewYco is Yco+1,
        write_XY_to_store(T,Xco,NewYco)
      /*  elementX((Xco,Yco),[1,2,3,4,5,6,7,8,9]),insert, % Here it is needed.
        elementY((Xco,Yco),[1,2,3,4,5,6,7,8,9]),insert,
        elementX((Xco,H),[Xco]),
        elementY((Xco,H),[Yco]),
        NewYco is Yco+1,
        write_XY_to_store(T,Xco,NewYco)*/
   ).


block_constraint(Xco,Yco,SXco,SYco) :-
     RowFirst is div((Xco-1),3),
     RowSecond is div((SXco-1),3),
     ColFirst is div((Yco-1),3),
     ColSecond is div((SYco-1),3),
     RowFirst =:= RowSecond,
     ColFirst =:= ColSecond.


isEqual(Xco,Yco,SXco,SYco) :-
      Xco =:= SXco,
      Yco =:= SYco.




rem_dup @ insert,elementY((Xco,Yco),[_]) \elementY((Xco,Yco),List) #passive
                                                      <=>
                                                      length(List,N),
                                                      N > 1|true.

rem_dup @ insert,elementX((Xco,Yco),[_]) \ elementX((Xco,Yco),List) #passive
                                                  <=>
                                                  length(List,N),
                                                  N > 1|true.

insert <=> nop.
clean_nop \ insert <=> true.
clean_nop \ nop <=> true.

%Same constraints are used as were used in eclipse implementation.(First constraint from the eclipse implementation).
unique_columnX @ elementX((Xco,Yco),[V]) , elementX((SXco,Yco),[V]) #passive <=> (Xco \= SXco)|false.
unique_columnY @ elementY((Xco,Yco),[V]) , elementY((SXco,Yco),[V]) #passive <=> (Xco \=SXco)|false.



%No two elements should map to same position.
unique_positions @elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementX((FXco,FYco),[V2]) , elementY((FXco,FYco),[V2Y]) #passive
                                                                                         <=> (Xco \= FXco ; Yco \= FYco),isEqual(V1,V1Y,V2,V2Y) |false.


%Block Constraint
unique_positions @elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementX((DXco,Yco),[V2]), elementY((DXco,Yco),[V2Y]) #passive
                                                                              <=> (DXco \= Xco ),
                                                                                   block_constraint(V1,V1Y,V2,V2Y) |false.







iterate_outerY_list(_,_,_,[],[]).

iterate_outerY_list(X,Y,X2,[Y2|T],L2) :-
   \+block_constraint(X,Y,X2,Y2),
    iterate_outerY_list(X,Y,X2,T,L2).


iterate_outerY_list(X,Y,X2,[Y2|T],[HS|TS]) :-
        block_constraint(X,Y,X2,Y2),
          HS = Y2,
          iterate_outerY_list(X,Y,X2,T,TS).


getInvalidYValues(X,Y,X2,ListS,Result) :-
       iterate_outerY_list(X,Y,X2,ListS,Result).




iterate_outerX_list(_,_,[],[]).
iterate_outerX_list(X,Y,[H|T],[R|TS]) :-
    block_constraint(X,Y,H,Y),
    R = H,
    iterate_outerX_list(X,Y,T,TS).


iterate_outerX_list(X,Y,[H|T],Result) :-
      \+block_constraint(X,Y,H,Y),
      iterate_outerX_list(X,Y,T,Result).






getInvalidXValues(X,Y,ListS,Result) :-
    iterate_outerX_list(X,Y,ListS,Result).



remove_invalid_values(L1,L2,RestF,RestS,NewRestF,NewRestS) :-
      flatten(RestF,FRestF),
      flatten(RestS,SRestS),
      subtract(L1,FRestF,NewRestF),
      subtract(L2,SRestS,NewRestS).

iterate(_,_,_,[],[],[]).

iterate(X,Y,X2,[Y2|T],L1,L2) :-
    \+isEqual(X,Y,X2,Y2),
      iterate(X,Y,X2,T,L1,L2).


iterate(X,Y,X2,[Y2|T],[H|T2],[HS|TS]) :-
        isEqual(X,Y,X2,Y2),
        H = X2,
        HS = Y2,
      iterate(X,Y,X2,T,T2,TS).


getInvalidEqualValues(_,_,[],_,[],[]).
getInvalidEqualValues(X,Y,[H|T],ListS,[HF|TF],[HS|TS]) :-
             iterate(X,Y,H,ListS,HF,HS),
        getInvalidEqualValues(X,Y,T,ListS,TF,TS).





%Propagation

remove_from_columnX @ refine, elementX((Xco,Yco),[Val]) \elementX((SXco,Yco),List) <=>
                                                                                (Xco \= SXco),
                                                                                 length(List,N),
                                                                                 N > 1,
                                                                                 select(Val,List,Rest)|elementX((SXco,Yco),Rest).


remove_from_columY @refine,elementY((Xco,Yco),[Val]) \elementY((SXco,Yco),List) <=>
                                                                               (Xco \= SXco),
                                                                                length(List,N),
                                                                                N > 1,
                                                                                select(Val,List,Rest)|elementY((SXco,Yco),Rest).



unique_positions @elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementX((FXco,FYco),[V1]) \elementY((FXco,FYco),ListSecond) #passive
                                                                                           <=> (Xco \= FXco ; Yco \= FYco),
                                                                                               member(V1Y,ListSecond),
                                                                                               select(V1Y,ListSecond,RestSecond)|(RestSecond = [] -> false ;elementY((FXco,FYco),RestSecond)).




unique_positions @elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementY((FXco,FYco),[V1Y]) \elementX((FXco,FYco),ListSecond) #passive
                                                                                                <=> (Xco \= FXco ; Yco \= FYco),
                                                                                                    member(V1,ListSecond),
                                                                                                    select(V1,ListSecond,RestSecond)|(RestSecond = [] -> false ;elementX((FXco,FYco),RestSecond)).




unique_block_Y @ elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementX((FXco,Yco),[V1])\elementY((FXco,Yco),List) <=>
                                                                                                     (FXco \= Xco),
                                                                                                      length(List,N),
                                                                                                      N > 1,
                                                                                                      getInvalidYValues(V1,V1Y,V1,List,Rest),
                                                                                                      flatten(Rest,First),
                                                                                                      length(First,M),
                                                                                                      M > 1,
                                                                                                      subtract(List,First,R)|(R = [] -> false ;elementY((FXco,Yco),R)).




unique_block_X @elementX((Xco,Yco),[V1]),elementY((Xco,Yco),[V1Y]),elementY((FXco,Yco),[V1Y])\elementX((FXco,Yco),List) <=>
                                                                                                        (FXco \= Xco),
                                                                                                        length(List,N),
                                                                                                        N > 1,
                                                                                                        getInvalidXValues(V1,V1Y,List,Rest),
                                                                                                        flatten(Rest,First),
                                                                                                        length(First,M),
                                                                                                        M > 1,
                                                                                                        subtract(List,First,R)|(R = [] -> false;elementX((FXco,Yco),R)).










refine <=> search(2).
launch_search @ search(N),elementY((Xco,Yco), Vs) # passive
                              <=> length(Vs, Len), Len =:= N | member(V, Vs), elementY((Xco,Yco), [V]), refine.
launch_search @ search(N),elementX((Xco,Yco), Vs) # passive
                                <=> length(Vs, Len), Len =:= N |member(V, Vs),elementX((Xco,Yco), [V]), refine.

search(9) <=> true.
search(N) <=> NN is N + 1, search(NN).


cleanup \ elementX(_, _) <=> true.
cleanup \ elementY(_, _) <=> true.
cleanup <=> true.
