:- use_module(library(chr)).
:- compile(sudex_toledo).
:- chr_type list(T) ---> [] ; [T|list(T)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint element(+coordinates,+values).
:- chr_constraint elementS(+coordinates,+values).
:- chr_constraint refine/0,start_refine/1,clean_store/0,insert/0,clean_insert/0,nop.


solve_chan(Name) :-
   puzzles(Puzzle,Name),
   write_data_to_store(Puzzle,1),
   build_constraint_store(Puzzle,1),
   clean_insert,
   refine.








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




write_elem([],_,_).
write_elem([H|T],X,Y) :-
       var(H),
       element((X,Y),[1,2,3,4,5,6,7,8,9]),insert,
       NewY is Y+1,
       write_elem(T,X,NewY).

write_elem([H|T],X,Y) :-
       \+var(H),
       element((X,Y),[1,2,3,4,5,6,7,8,9]),
       element((X,H),[Y]),insert,
       NewY is Y+1,
       write_elem(T,X,NewY).





write_data_to_store([],_).
write_data_to_store([H|T],X) :-
    write_elem(H,X,1),insert,
    NewX is X+1,
    write_data_to_store(T,NewX).


build_constraint_store([],_).
build_constraint_store([Head|Tail],Xco) :-
      write_to_store(Head,Xco,1),
      NewXco is Xco+1,
      build_constraint_store(Tail,NewXco).



block_constraint(Xco,Yco,SXco,SYco) :-
     RowFirst is div((Xco-1),3),
     RowSecond is div((SXco-1),3),
     ColFirst is div((Yco-1),3),
     ColSecond is div((SYco-1),3),
     RowFirst =:= RowSecond,
     ColFirst =:= ColSecond.



getInvalidValues(X,Y,Xco,List,Result) :-
      getValues(X,Y,Xco,List,Result).


getValues(_,_,_,[],[]).
getValues(X,Y,Xco,[H|T],[H|T2]) :-
     block_constraint(X,Y,Xco,H),
     getValues(X,Y,Xco,T,T2).


 getValues(X,Y,Xco,[H|T],List) :-
     \+block_constraint(X,Y,Xco,H),
     getValues(X,Y,Xco,T,List).


sat_block_constraint(XcoFirst,XcoSecond,YcoFirst,YcoSecond):-
         RowFirst is div((XcoFirst-1),3),
         RowSecond is div((XcoSecond-1),3),
         ColFirst is div((YcoFirst-1),3),
         ColSecond is div((YcoSecond-1),3),
         RowFirst =:= RowSecond,
         ColFirst =:= ColSecond.



remove_invalid_values(List1,List2,Rest) :-
     subtract(List1,List2,Rest).


%------------------Insertion Help constraints-----------------------------------------------------------------------------------------------

rem_insert_mistakes @ insert, element((X,Y),[_]) \ element((X,Y),List) #passive
                                                                         <=> length(List,N),
                                                                             N > 1 |true.
insert <=> nop.

clean_insert \ nop <=> true.


%------------------------------------------------------------Sudoku channeling constraints-------------------------------------------------------

channeling @ elementS((X,Y),[E]) \ element((X,E),[V]) #passive <=>
                                                     (V \= Y) |false.

channeling @ element((X,E),[Y]) \ elementS((X,Y),[V]) #passive <=>
                                            (E \= V) |false.


channeling_remove_elem @ refine,elementS((X,Y),[E]) \element((X,E),List) <=>
                                                               length(List,N),
                                                               N > 1,
                                                               member(Y,List) | element((X,E),[Y]).

channeling_remove_elem @ refine,element((X,E),[Y]) \elementS((X,Y),List) <=>
                                                               length(List,M),
                                                               M > 1,
                                                               member(E,List) | elementS((X,Y),[E]).






%--------------------------------------------------------------------simple sudoku constraints ---------------------------------------------------
%Unique in row column and block with one element left.
unique_row @ elementS((Xco,Yco),[Val]) \ elementS((Xco,DYco),[Val]) #passive <=> (Yco \= DYco) |false.
unique_colomn @ elementS((Xco,Yco),[Val]) \elementS((DXco,Yco),[Val]) #passive <=> (Xco \= DXco) |false.
unique_block @ elementS((Xco,Yco),[Val]) \ elementS((DXco,DYco),[Val]) #passive <=> (Xco \= DXco ; Yco \= DYco),sat_block_constraint(Xco,DXco,Yco,DYco)|false.

%Propagation rules
row_search @ refine,elementS((Xco,_),[Val]) \ elementS((Xco,DYco),List)
                                                             <=> length(List,N),
                                                                 N >= 2 ,
                                                                 select(Val,List,Rest) | elementS((Xco,DYco),Rest).
col_search @ refine,elementS((_,Yco),[Val]) \ elementS((DXco,Yco),List)
                                                             <=> length(List,N),
                                                                  N >= 2 ,
                                                                 select(Val,List,Rest) | elementS((DXco,Yco),Rest).
unique_block @ refine,elementS((Xco,Yco),[Val]) \ elementS((DXco,DYco),List)
                                                             <=> (Xco \= DXco ; Yco \= DYco),
                                                                  length(List,N),
                                                                  N >= 2,
                                                                  select(Val,List,Rest) ,
                                                                  sat_block_constraint(Xco,DXco,Yco,DYco)| elementS((DXco,DYco),Rest).

%-----------------------------------------------------------Sudoku alt constraints---------------------------------------------------------

row_const @ element((X,Y),[V]) \ element((X,Yco),[V]) #passive
                                                            <=> (Y \= Yco) | false.
column_const @ element((X,Y),[V]) \ element((Xco,Y),[V]) #passive
                                                         <=> (Xco \= X) |false.


block_const @element((X,Y),[V]) \ element((Xco,Y),[V1]) #passive
                                                  <=> (Xco \= X),
                                                       block_constraint(X,V,Xco,V1)|false.






refine_row @ refine, element((X,Y),[V]) \ element((X,Yco),List) <=>
                                          length(List,N),
                                          (Y \= Yco),
                                          N > 1,
                                          %member(V,List),!,
                                          select(V,List,Rest)|(Rest = [] -> false ; element((X,Yco),Rest)).


refine_colum @ refine,element((X,Y),[V]) \ element((Xco,Y),List) <=>
                                                            length(List,N),
                                                            (X \= Xco),
                                                            N > 1,
                                                            member(V,List),
                                                            select(V,List,Rest) |(Rest = [] -> false;element((Xco,Y),Rest)).


refine_bloc @ refine,element((X,Y),[V]) \ element((Xco,Y),List) <=>
                                                            (X \= Xco),
                                                            length(List,N),
                                                            N > 1,
                                                            getInvalidValues(X,V,Xco,List,Rest),
                                                            length(Rest,M),
                                                            M > 0,
                                                            remove_invalid_values(List,Rest,NewRest)|((Rest = [] ;NewRest = []) -> false;element((Xco,Y),NewRest)).


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------




refine <=> start_refine(2).
launch_search @ start_refine(N),element((Xco,Yco), List) # passive
             <=> length(List, L), L =:= N | member(M, List), element((Xco,Yco), [M]), refine.

launch_search @ start_refine(N),elementS((Xco,Yco), List) # passive
                <=> length(List, L), L =:= N | member(M, List), elementS((Xco,Yco), [M]), refine.



start_refine(9) <=> true.
start_refine(N) <=> NN is N + 1, start_refine(NN).
