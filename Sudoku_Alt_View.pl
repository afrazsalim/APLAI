:- lib(ic).
:- lib(ic_global).
:- import alldifferent/1 from ic_global.
:- compile("sudex_toledo").


solve(Puzzle,Name) :-
  puzzles(Temp,Name),
  listToArray(Temp,Lists),
  array_list(Puzzle,Lists),
  create_model(Puzzle,Xcos,Ycos),
  constraints(Xcos,Ycos),
  search_board(Xcos),
  search_board(Ycos),
  convert_to_board(Xcos,Ycos,Puzzle),
  print_result(Puzzle).



%convert_board_to_array(Board,Puzzle) :-



convert_to_board(Xcos,Ycos,Puzzle) :-
    (
      multifor([I,J],[1,1],[9,9]),param(Xcos,Ycos,Puzzle) do
        subscript(Xcos,[I,J],X),
        subscript(Ycos,[I,J],Y),
        subscript(Puzzle,[X,Y],J)
    ).




constraints(Xco,Yco) :-
  (
   for(I,1,9) ,
     param(Xco,Yco) do
     Row is Xco[1..9,I],
     Col is Yco[1..9,I],
     alldifferent(Row),
     alldifferent(Col)
   ),
   (
      multifor([S,T],[1,1],[9,9]),foreach(R,Regions),param(Xco,Yco) do  %All entries must be different. This constraint can be left out but
        Xco[S,T]*9+Yco[S,T] #=R  % but it effects the run-time dramatically.
   ),
   alldifferent(Regions),
   block_constraint(Yco).


%We only need y-coordinates here to impose constraint because these are the y-coordinates that shift the value to right or left
% in same row.
block_constraint(Yco) :-
   TotalBlock is 3,
   N is 9,
  ( multifor([B,K],[1,1],[TotalBlock,N]), param(Yco,TotalBlock) do
        BeginIndex is (B-1)*TotalBlock+1,
        EndIndex is (B*TotalBlock),
      ( for(X,BeginIndex,EndIndex), foreach(B,Blocks), param(Yco,K,TotalBlock) do
          B #:: 0..TotalBlock-1,
          Shift #:: 1..TotalBlock,
          Yco[X,K] #= B*TotalBlock + Shift
      ),
      alldifferent(Blocks)
  ).


print_result(Result) :-
    (
    foreachelem(E,Result,[_,J])
    do
    (J =:= 1 -> nl; true),
    write(" "),
    ( var(E) -> write("_") ; write(E))
  ).




create_model(Puzzle,Xco,Yco) :-
   N is 9,
   dim(Xco,[N,N]),
   dim(Yco,[N,N]),
   Xco[1..N,1..N] :: [1..N],
   Yco[1..N,1..N] :: [1..N],
   (
      multifor([I,J],[1,1],[N,N]),param(Xco,Yco,Puzzle) do
      subscript(Puzzle,[I,J],R),
      write(R),
      (
      var(R) -> true
      ;
      (subscript(Xco,[I,R],I),
      subscript(Yco,[I,R],J))
      )

   ).

search_board(Result) :-
  term_variables(Result,Vars),
  labeling(Vars).



  listToArray([],[]).
  listToArray([A|As],[B|Bs]) :-
      array_list(B,A),
      listToArray(As,Bs).



problem(1, [](
          [](_, _, 2, _, _, 5, _, 7, 9),
          [](1, _, 5, _, _, 3, _, _, _),
          [](_, _, _, _, _, _, 6, _, _),
          [](_, 1, _, 4, _, _, 9, _, _),
          [](_, 9, _, _, _, _, _, 8, _),
          [](_, _, 4, _, _, 9, _, 1, _),
          [](_, _, 9, _, _, _, _, _, _),
          [](_, _, _, 1, _, _, 3, _, 6),
          [](6, 8, _, 3, _, _, 4, _, _))).
