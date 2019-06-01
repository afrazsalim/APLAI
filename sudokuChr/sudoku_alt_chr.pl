:- use_module(library(chr)).
:- compile("sort").
:- compile("sudex_toledo").
:- chr_type list(L) ---> [] ; [L|list(L)]. %Back to Haskell.
:- chr_type xco == natural.
:- chr_type yco == natural.
:- chr_type coordinates ---> (xco,yco).
:- chr_type values ---> [natural|list(natural)].
:- chr_constraint tuple(+coordinates,+values).
:- chr_constraint refine/0,insert/0,start_refine/1,clean_store/0,remove_insert/0.


solve(Name) :-
   puzzles(Puzzle,Name),
   write_data_to_store(Puzzle,1),
   remove_insert,
   refine,
   store_element_to_list(1,Result),
  % clean_store,
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
      find_chr_constraint(tuple((X,Y),[R])),
      Coordinate is R*9+Y,
      Rec = Coordinate-X,
      H = Rec,
      NewY is Y+1,
  read_elements(X,NewY,T).


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


%-------------------------------------------------------------Constraints------------------------------------------------------------------
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




refine <=> start_refine(2).

launch_search @ start_refine(T),tuple((X,Y),List)#passive <=>
                                                  length(List,N),
                                                  N =:= T|member(P,List),tuple((X,Y),[P]),refine.


start_refine(9) <=> true.
start_refine(N) <=> NewN is N+1,start_refine(NewN).

clean_store \ tuple(_,_) <=> true.
