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
  remove_insert.






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
/*
channeling @ tuple((E,Y),[V]) \ elementS((V,Y),[E]) #passive <=>
                                            (E \= V) |false.
*/

channeling_remove_elem @ refine,elementS((X,Y),[E]) \tuple((E,Y),List) <=>
                                                               length(List,N),
                                                               N > 1,
                                                               member(X,List) | tuple((X,E),[Y]).
/*
channeling_remove_elem @ refine,tuple((E,Y),[X]) \ elementS((X,Y),List) <=>
                                                               length(List,M),
                                                               M > 1,
                                                               member(E,List) | elementS((X,Y),[E]).
*/
