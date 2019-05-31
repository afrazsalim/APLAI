:- use_module(library(chr)).
:- compile("sudex_toledo").
:- chr_type list(T) ---> [] ; [T|list(T)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint elementS(+coordinates,+values).
:- chr_constraint refine/0,start_refine/1,cleanup/0.


solve(Name) :-
  puzzles(Puzzle,Name),
  once(time(solve_for_once(Puzzle))),
  print_board(1,List),
  cleanup,
  write_out(List,1).



write_out([],_).
write_out([H|T],I) :-
  print_out(H),
  writeln(" "),
  NewI is I+1,
  write_out(T,NewI).


print_out([]).
print_out([H|T]) :-
  write(H),
  write(" "),
  print_out(T).

print_board(10,[]).
print_board(X,[NewList|List]) :-
  add_element_to_list(X,1,NewList),
  NewX is X+1,
  print_board(NewX,List).

add_element_to_list(_,10,[]).
add_element_to_list(X,Y,[V|T]) :-
  find_chr_constraint(elementS((X,Y),[V])),
  NewY is Y+1,
  add_element_to_list(X,NewY,T).



solve_for_once(Puzzle) :-
  build_constraint_store(Puzzle,1),
  refine.


build_constraint_store([],_).
build_constraint_store([Head|Tail],Xco) :-
     write_to_store(Head,Xco,1),
     NewXco is Xco+1,
     build_constraint_store(Tail,NewXco).


write_to_store([],_,_).
write_to_store([H|T],Xco,Yco) :-
    (
      var(H) ->
         (elementS((Xco,Yco),[1,2,3,4,5,6,7,8,9]),
         NewYco is Yco+1,
         write_to_store(T,Xco,NewYco))
         ;
         (elementS((Xco,Yco),[H]),
          NewYco is Yco +1,
          write_to_store(T,Xco,NewYco))
    ).


sat_block_constraint(XcoFirst,XcoSecond,YcoFirst,YcoSecond):-
            RowFirst is div((XcoFirst-1),3),
            RowSecond is div((XcoSecond-1),3),
            ColFirst is div((YcoFirst-1),3),
            ColSecond is div((YcoSecond-1),3),
            RowFirst =:= RowSecond,
            ColFirst =:= ColSecond.



%Unique in row column and block with one element left.
unique_row @ elementS((Xco,Yco),[Val]) \ elementS((Xco,DYco),[Val]) #passive <=> (Yco \= DYco) |false.
unique_colomn @ elementS((Xco,Yco),[Val]) \elementS((DXco,Yco),[Val]) #passive <=> (Xco \= DXco) |false.
unique_block @ elementS((Xco,Yco),[Val]) \ elementS((DXco,DYco),[Val]) #passive <=> (Xco \= DXco ; Yco \= DYco),sat_block_constraint(Xco,DXco,Yco,DYco)|false.

%Propagation rules
row_search @ refine,elementS((Xco,_),[Val]) \ elementS((Xco,DYco),List)
                                                             <=> length(List,N),
                                                                 N >= 2 ,
                                                                 member(Val,List),
                                                                 delete(List,Val,Rest) | elementS((Xco,DYco),Rest).
col_search @ refine,elementS((_,Yco),[Val]) \ elementS((DXco,Yco),List)
                                                             <=> length(List,N),
                                                                  N >= 2 ,
                                                                 member(Val,List),
                                                                 delete(List,Val,Rest) | elementS((DXco,Yco),Rest).
unique_block @ refine,elementS((Xco,Yco),[Val]) \ elementS((DXco,DYco),List)
                                                             <=> (Xco \= DXco ; Yco \= DYco),
                                                                  length(List,N),
                                                                  N >= 2,
                                                                  member(Val,List),
                                                                  delete(List,Val,Rest) ,
                                                                  sat_block_constraint(Xco,DXco,Yco,DYco)| elementS((DXco,DYco),Rest).




refine <=> start_refine(2).
launch_search @ start_refine(N),elementS((Xco,Yco), List) # passive
             <=> length(List, L), L =:= N | member(M, List), elementS((Xco,Yco), [M]), refine.

start_refine(9) <=> true.
start_refine(N) <=> NewVal is N + 1, start_refine(NewVal).

cleanup \ elementS(_, _) <=> true.
cleanup <=> true.
