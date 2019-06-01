:- lib(ic).
:- lib(ic_global).
:- import alldifferent/1 from ic_global.
:- compile(sudex_toledo).
:- compile(helper_functions).

%--------------------------------------------------------------------Classic view constraints -------------------------------------
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






%---------------------------------------------------------------------Alt view constraints-----------------------------------------------



impose_constraint_alt(Model) :-
   dim(Model,[N,N]),
   Model[1..N,1..N] :: [1..N],
  (for(I,1,N), param(Model,N) do
     Rw is Model[I,1..N],
     Cl is Model[1..N,I],
     alldifferent(Rw),
     alldifferent(Cl)
  ),
  block_constraint(Model).






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
