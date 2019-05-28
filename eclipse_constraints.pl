:- lib(ic).
:- lib(ic_global).
:- import alldifferent/1 from ic_global.
:- compile(sudex_toledo).
:- compile(helper_functions).

%Only used for channeling.
outer_call_simple(Puzzle) :-
	 impose_constraint(Puzzle).

impose_constraint(R) :-
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




%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Constraints for alternative view


%This function is only used for channeling.
outer_call_alt(Puzzle,Result) :-
     build_model(Puzzle,Result),
     constrain_model(Result).


build_model(Puzzle,Model) :-
	      dim(Puzzle,[N,N]),
	      dim(Model,[N,N]),
        Model[1..N,1..N] :: [1..9],
	     (
	      multifor([I,J],[1,1],[N,N]),param(Puzzle,Model) do
	        subscript(Puzzle,[I,J],Col),
		   (
		      var(Col) -> true
		      ;
		      subscript(Model,[I,Col],J)
		       )
		    ).



block_constraint(Model) :-
      TotalBlock is 3,
      dim(Model,[N,N]),
     ( for(BlockNumber,1,TotalBlock),param(Model,TotalBlock,N) do
          for(J,1,N),param(Model,TotalBlock,BlockNumber)do
           BeginIndex is (BlockNumber-1)*TotalBlock+1,
           EndIndex is (BlockNumber*TotalBlock),
         ( for(I,BeginIndex,EndIndex), foreach(B,Blocks), param(Model,J,TotalBlock) do
             B #:: 0..TotalBlock-1,
             Shift #:: 1..TotalBlock,
             Model[I,J] #= B*TotalBlock + Shift
         ),
         alldifferent(Blocks)
     ).



constrain_model_alt(Model) :-
   dim(Model,[N,N]),
   block_constraint(Model), %Impose the block constraint.
   (
   for(I,1,N),param(N,Model) do
      Col is Model[1..N,I],
      Row is Model[I,1..N],
      alldifferent(Col),  %Ensure that every number has different Y-ccordinates.
      alldifferent(Row)  %Ensure that every row has different number.
   ).
