:- use_module(library(chr)).
:- compile("sudex_toledo").
:- compile("sort").
:- chr_type list(T) ---> [] ; [T|list(T)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint element(+coordinates,+values).
:- chr_constraint refine/0,start_refine/1,clean_store/0,insert/0,clean_insert/0,nop.

solve(Name) :-
   puzzles(Puzzle,Name),
   write_data_to_store(Puzzle,1),
   clean_insert,
   refine,
   store_element_to_list(1,Result),
   clean_store,
   flatten(Result,R),
   insert_sort(R,Temp),
   print_out(Temp).


print_out([]).
print_out([P-N|T]) :-
     write(N),
     write(" "),
     (
      P mod 9 =:= 0 -> writeln(" ") ; true
     ),
     print_out(T).




store_element_to_list(10,[]).
store_element_to_list(X,[H|T]) :-
     read_elements(X,1,H),
     NewX is X+1,
     store_element_to_list(NewX,T).

read_elements(_,10,[]).
read_elements(X,Y,[H|T]) :-
   find_chr_constraint(element((X,Y),[R])),
   Coordinate is X*9+R,
   Rec = Coordinate-Y,
   H = Rec,
   NewY is Y+1,
   read_elements(X,NewY,T).

write_data_to_store([],_).
write_data_to_store([H|T],X) :-
   write_elem(H,X,1),insert,
   NewX is X+1,
   write_data_to_store(T,NewX).

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


remove_invalid_values(List1,List2,Rest) :-
    subtract(List1,List2,Rest).


%------------------Insertion Help constraints-----------------------------------------------------------------------------------------------

rem_insert_mistakes @ insert, element((X,Y),[_]) \ element((X,Y),List) #passive
                                                                   <=> length(List,N),
                                                                       N > 1 |true.
insert <=> nop.

clean_insert \ nop <=> true.

%---------------------------------Constraints for row, column and block ---------------------------------------------------------------------

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






refine <=> start_refine(2).

launch_search @ start_refine(T),element((X,Y),List)#passive <=>
                                  length(List,N),
                                  N =:= T|member(P,List),element((X,Y),[P]),refine.


start_refine(9) <=> true.
start_refine(N) <=> NewN is N+1,start_refine(NewN).

clean_store \ element(_,_) <=> true.
