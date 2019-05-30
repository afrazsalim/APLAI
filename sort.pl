
insert_sort(List,Result):-
   insert_sort(List,[],Result).

insert_sort([],L,L):-
  !.

insert_sort([H|T],ACC,Result):-
   insert(H,ACC,InsertedList),
   insert_sort(T,InsertedList,Result).


insert(Element,[],[Element]):-
   !.

insert(PF-NN,[P-N],InsertedList):-
     (
	   P > PF ->
	   InsertedList = [PF-NN,P-N]
	   ;
	   InsertedList = [P-N,PF-NN]
	   ).

insert(PF-NF,[P-N|T],InsertedList):-
    (
	 PF > P ->
	    InsertedList = [P-N|NewList],
		insert(PF-NF,T,NewList)
		;
		InsertedList = [PF-NF,P-N|T]
	).
