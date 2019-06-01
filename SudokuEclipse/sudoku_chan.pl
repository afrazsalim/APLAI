:- compile(constraints).
:- compile(helper_functions).


solve(Name) :-
	puzzles(Puzzle,Name),
	convert_to_array(Model,Puzzle),
	constrain_classic_model(Model),
  read_puzzle(Model,AltModel),
  impose_constraint_alt(AltModel),
  create_Variable_list(Model,AltModel,Result),
  channeling_constraint(Model,AltModel),
  search_board(Result),
  print_result(Model).



create_Variable_list(Puzzle,Model,Result) :-
    array_flat(1,Puzzle,FR),
    array_flat(1,Model,SR),
    term_variables(FR,V1),
    term_variables(SR,V2),
    append(V1,V2,Result).


constrain_classic_model(R) :-
    	dim(R,[N,N]),
    	R #:: 1..N,
    	(for(I,1,N), param(R,N)
    	do
    		Rw is R[I,1..N],
    		Cl is R[1..N,I],
    		alldifferent(Rw),
    		alldifferent(Cl)
    	),
    	sqrt(N,Sq),
    	integer(Sq,ISq),
    	(multifor([J,K],[1,1],[7,7],[ISq,ISq]), param(R)
    	do
    		Cube is R[J..J+2,K..K+2],
    		flatten(Cube,LCube),
    		alldifferent(LCube)
    	).




channeling_constraint(Model,AltModel) :-
         dim(Model,[N,N]),
        (
        multifor([I,J] ,[1,1],[N,N]),param(N,Model,AltModel) do
            E #:: 1..9,
            Model[I,J] #= E,
            element(E,AltModel[1..N,J],I)
        ).


















search_board(Result) :-
   term_variables(Result,Vars),
   labeling(Vars).





print_result(Result) :-
        (
        foreachelem(E,Result,[_,J])
        do
        (J =:= 1 -> nl; true),
        write(" "),
        ( var(E) -> write("_") ; write(E))
      ).




block_constraint(Model) :-
        BlockSize is 3,
        dim(Model,[N,N]),
       ( for(BlockNumber,1,BlockSize),param(Model,BlockSize,N) do
            for(J,1,N),param(Model,BlockSize,BlockNumber)do
             BeginIndex is (BlockNumber-1)*BlockSize+1,
             EndIndex is (BlockNumber*BlockSize),
           ( for(I,BeginIndex,EndIndex), foreach(B,Blocks), param(Model,J,BlockSize) do
               B #:: 0..BlockSize-1,
               Shift #:: 1..BlockSize,
               Model[J,I] #= B*BlockSize + Shift
           ),
           alldifferent(Blocks)
       ).



convert_model(Model,Result) :-
   dim(Model,[N,N]),
   dim(Result,[N,N]),
   (
    multifor([I,J],[1,1],[N,N]),param(Model,Result) do
    subscript(Model,[I,J],Data),
    subscript(Result,[Data,J],I)
   ).
