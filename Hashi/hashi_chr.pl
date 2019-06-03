:- use_module(library(chr)).
:- use_module(library(lists)).
%:- compile("hashi_benchmarks").
:- op(700, xfx, 'in').
:- op(700, xfx, 'le').
:- op(700, xfx, 'eq').
%:- op(600, xfx, '..').
:- chr_constraint le/2, in/2, eq/2, add/3.


%inconsistency @ X in A..B <=> A > B | fail.
%intersect @ X in A..B, X in C..D <=> X in max(A,C)..min(B,D).

inconsistency @ _ in [] <=> fail.
intersect @ X in L1, X in L2 <=> intersection(L1, L2, L3) | X in L3.
in @ X in [X1] <=> X = X1.

:- chr_constraint solve/1, make_board/4, island/3, nesw/3, on_bounds/3, size/1, nbridge/3, ebridge/3, sbridge/3, wbridge/3, refine/0.

%le @ X le Y, Y in C..D \ X in A..B <=> B > D | X in A..D.
%le @ X le Y, X in A..B \ Y in C..D <=> C < A | Y in A..D.

/*
solve(Id) :-
	puzzle(Id, Size, Puzzle),
	size(Size),
	make_board(Puzzle, Size, Size, Size),
	refine
	%make_NESW(Size)
	%print_board(Size, Size)
	.
*/
solve(Id) <=> puzzle(Id, Size, Puzzle), size(Size), make_board(Puzzle, 1, 1, Size), refine.

% ------------- ADD ISLANDS TO STORE ----------------
make_board(Puzzle, I, J, Size) <=> member((I,J,N), Puzzle), J < Size+1, I < Size + 1, NJ is J + 1 | island(I,J,N), make_board(Puzzle, I, NJ, Size).
make_board(Puzzle, I, J, Size) <=> no_member((I,J,_), Puzzle), J < Size+1, I < Size + 1, NJ is J + 1| island(I,J,0), make_board(Puzzle, I, NJ, Size).
make_board(Puzzle, I, NJ, Size) <=> NJ =:= Size + 1, NI is I + 1 | make_board(Puzzle, NI, 1, Size).
make_board(_, NI, _, Size) <=> NI =:= Size + 1 | true.

no_member((I,J,N), Puzzle) :-
	(member((I,J,N), Puzzle) ->
		false
	;
		true
	).

%------------ EXAMPLE --------------
puzzle(0, 4, [(1,1,4), (1,4,4), (4,1,4), (4,4,4)]).

%--------------- INIT -----------------------------------
% also constraint 4 (at most 2 bridges).
init_bridges @ island(I,J,0) ==> 	N in [0,1,2], nbridge(I,J,N), S in [0,1,2], sbridge(I,J,S), 
									E in [0,1,2], ebridge(I,J,E), W in [0,1,2], wbridge(I,J,W).

% The sum of bridges on island or not must equal K (0 if no island).


%------------- NESW CONSTRAINTS -------------------------
% bridges can only go horizontally or vertically
nesw1_vert @ island(I,J,0), nbridge(I,J,N), sbridge(I,J,S), ebridge(I,J,E), wbridge(I,J,W) ==>  N in [1,2], S in [1,2] | E in [0], W in [0].
nesw1_hor @ island(I,J,0), nbridge(I,J,N), sbridge(I,J,S), ebridge(I,J,E), wbridge(I,J,W) ==>  E in [1,2], W in [1,2] | N in [0], S in [0].

nesw2_N @ island(I,J,_), nbridge(I,J,N) 			==> N in [1,2], NI is I - 1, NI > 1, sbridge(NI, J, NS)	 	| NS in N .
nesw2_S @ island(I,J,_), sbridge(I,J,S), size(Size) ==> S in [1,2], NI is I + 1, NI < Size, nbridge(NI, J, NN) 	| NN in S.
nesw2_E @ island(I,J,_), ebridge(I,J,E), size(Size) ==> E in [1,2], NJ is J + 1, NJ < Size, wbridge(I, NJ, NW) 	| NW in E.
nesw2_N @ island(I,J,_), wbridge(I,J,W)				==> W in [1,2], NJ is J - 1, NJ > 1, ebridge(I, NJ, NE)		| NE in W.

nesw3 @ island(I,J,0) \ nbridge(I,J,N), ebridge(I,J,E) <=> N in [1,2], E in [1,2] | fail.

% constraint 5
add_nesw_island @ refine, island(I,J,K) \ nbridge(I,J,N), sbridge(I,J,S), ebridge(I,J,E), wbridge(I,J,W) <=>  N + E + S + W = K | nesw(I,J,[N,E,S,W]).

% faster terms
/*
island_8 	@ island(I, J, 8), size(Size) \ nesw(I,J,[N,E,S,W]) 		
							<=> I > 1, I < Size, J > 1, J < Size | nesw(I, J, [2, 2, 2, 2]).
island_6_N 	@ island(1, J, 6), size(Size) \ nesw(1,J,[N,E,S,W]) 		
							<=> J > 1, J < Size | nesw(1, J, [0, 2, 2, 2]).
island_6_S 	@ island(Size, J, 6), size(Size) \ nesw(Size,J,[N,E,S,W])	
							<=> J > 1, J < Size | nesw(Size, J, [2, 2, 0, 2]).
island_6_W 	@ island(I, 1, 6), size(Size) \ nesw(I,1,[N,E,S,W]) 		
							<=> I > 1, I < Size | nesw(I, 1, [2, 2, 2, 0]).
island_6_E 	@ island(I, Size, 6), size(Size) \ nesw(I,Size,[N,E,S,W]) 	
							<=> I > 1, I < Size | nesw(I, Size, [2, 0, 2, 2]).
island_4_NW @ island(1,1,4) \ nesw(1,1,[N,E,S,W]) 						
							<=> N \= 0, E \= 2, S \= 2, W \= 0 | nesw(1,1,[0, 2, 2, 0]).
island_4_NE @ island(1,Size,4) \ nesw(1,Size,[N,E,S,W]) 				
							<=> N \= 0, E \= 0, S \= 2, W \= 2 | nesw(1,Size,[0, 0, 2, 2]).
island_4_SW @ island(Size,1,4) \ nesw(Size,1,[N,E,S,W]) 				
							<=> N \= 2, E \= 2, S \= 0, W \= 0 | nesw(Size,1,[2, 2, 0, 0]).
island_4_SE @ island(Size,Size,4) \ nesw(Size,Size,[N,E,S,W]) 			
							<=> N \= 2, E \= 0, S \= 0, W \= 2 | nesw(Size,Size,[2, 0, 0, 2]).
*/

%----------- PRINT ---------------
