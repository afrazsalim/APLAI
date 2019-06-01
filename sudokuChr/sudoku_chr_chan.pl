:- use_module(library(chr)).
:- compile("sort").
:- compile("sudex_toledo").
:- chr_type list(L) ---> [] ; [L|list(L)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint tuple(+coordinates,+values).
:- chr_constraint elementS(+coordinates,+values).
:- chr_constraint refine/0,insert/0,start_refine/1,clean_store/0,remove_insert/0.



solve(Name) :-
  puzzles(Puzzle,Name),
  build_constraint_store(Puzzle,1),
  write_data_to_store(Puzzle,1),
  remove_insert,
  refine,
  print_board(1,List),
  clean_store,
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



write_data_to_store([],_).
write_data_to_store([H|T],X) :-
        write_elem(H,X,1),
        NewX is X+1,
        write_data_to_store(T,NewX).

write_elem([],_,_).
write_elem([H|T],X,Y) :-
         var(H),
         tuple((X,Y),[1,2,3,4,5,6,7,8,9]),insert,
         NewY is Y+1,
         write_elem(T,X,NewY).

write_elem([H|T],X,Y) :-
         \+var(H),
         tuple((X,Y),[1,2,3,4,5,6,7,8,9]),
         tuple((H,Y),[X]),insert,
         NewY is Y+1,
         write_elem(T,X,NewY).

sat_block_constraint(XcoFirst,XcoSecond,YcoFirst,YcoSecond):-
           RowFirst is div((XcoFirst-1),3),
           RowSecond is div((XcoSecond-1),3),
           ColFirst is div((YcoFirst-1),3),
           ColSecond is div((YcoSecond-1),3),
           RowFirst =:= RowSecond,
           ColFirst =:= ColSecond.

block_constraint(Xco,Yco,SXco,SYco) :-
         RowFirst is div((Xco-1),3),
         RowSecond is div((SXco-1),3),
         ColFirst is div((Yco-1),3),
         ColSecond is div((SYco-1),3),
         RowFirst =:= RowSecond,
         ColFirst =:= ColSecond.


getInvalidValues(X,Y,Yco,List,Result) :-
        getValues(X,Y,Yco,List,Result).


getValues(_,_,_,[],[]).
     getValues(X,Y,Yco,[H|T],[H|T2]) :-
       block_constraint(X,Y,H,Yco),
     getValues(X,Y,Yco,T,T2).


getValues(X,Y,Yco,[H|T],List) :-
   \+block_constraint(X,Y,H,Yco),
   getValues(X,Y,Yco,T,List).

remove_invalid_values(List1,List2,Rest) :-
 subtract(List1,List2,Rest).


%---------------------------------------------------------------Insert help ----------------------------------------------------------

remove_dup @ insert,tuple((X,Y),[_]) \ tuple((X,Y),List) #passive <=>
                                                       length(List,N),
                                                       N > 1|true.


remove_insert \insert <=> true.


         %-------------------------------------------------------------Classiv view Constraints------------------------------------------------------------------

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


%------------------------------------------------------------------------Alt view constraints -------------------------------------------------------------------------
unique_row @ tuple((X,Y),[E]) \tuple((X,Yco),[E]) #passive <=>  (Y \= Yco) |false.

unique_column @ tuple((X,Y),[E]) \ tuple((Xco,Y),[E]) #passive <=> (X \= Xco)|false.


unique_block @ tuple((X,Y),[E]) \ tuple((X,Yco),[V]) #passive <=> (Y \= Yco),
                                                                  block_constraint(E,Y,V,Yco)|false.

%-----------------------------------------------------------Domain Reduction --------------------------------------------------------------

refine_row @ refine, tuple((X,Y),[V]) \ tuple((X,Yco),List) <=>
                                          length(List,N),
                                          (Y \= Yco),
                                          N > 1,
                                          %member(V,List),!,
                                          select(V,List,Rest)|(Rest = [] -> false ; tuple((X,Yco),Rest)).


refine_colum @ refine,tuple((X,Y),[V]) \ tuple((Xco,Y),List) <=>
                                                          length(List,N),
                                                          (X \= Xco),
                                                          N > 1,
                                                          member(V,List),
                                                          select(V,List,Rest) |(Rest = [] -> false;tuple((Xco,Y),Rest)).



refine_bloc @ refine,tuple((X,Y),[V]) \ tuple((X,Yco),List) <=>
                                                        (Y \= Yco),
                                                         length(List,N),
                                                         N > 1,
                                                         getInvalidValues(V,Y,Yco,List,Rest),
                                                         length(Rest,M),
                                                         M > 0,
                                                         remove_invalid_values(List,Rest,NewRest)|((Rest = [] ;NewRest = []) -> false;tuple((X,Yco),NewRest)).


%---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%--------------------------------------------------------------Channeling constraints----------------------------------------------------------------

channeling @ elementS((X,Y),[E]) \ tuple((E,Y),[V]) #passive <=>
                                                     (V \= X) |false.

 channeling @tuple((E,Y),[X]) \ elementS((X,Y),[V])  #passive <=>
                                              (V \= E) |false.



channeling_remove_elem @ refine,elementS((X,Y),[E]) \tuple((E,Y),List) <=>
                                                               length(List,N),
                                                               N > 1,
                                                              member(X,List) | tuple((E,Y),[X]).




channeling_remove_elem @ refine,tuple((E,Y),[X]) \ elementS((X,Y),List) <=>
                                                               length(List,M),
                                                               M > 1,
                                                               member(E,List) | elementS((X,Y),[E]).


%-----------------------------------------------------------------search on both variables -----------------------------------------------------------------------
refine <=> start_refine(2).

%Search on both models.

launch_search @ start_refine(T),tuple((X,Y),List)#passive <=>
                                                  length(List,N),
                                                  N =:= T|member(P,List),tuple((X,Y),[P]),refine.

launch_search @ start_refine(N),elementS((Xco,Yco), List) # passive
                                       <=> length(List, L), L =:= N | member(M, List), elementS((Xco,Yco), [M]), refine.



start_refine(9) <=> true.
start_refine(N) <=> NewN is N+1,start_refine(NewN).

clean_store \ tuple(_,_) <=> true.
clean_store \ elementS(_,_) <=> true.
