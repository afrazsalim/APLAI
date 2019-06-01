 % Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library
:- lib(tentative).

:- compile("hashi_benchmarks.pl"). % imports the given puzzles.
:- compile("helper_functions").

% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness

% NOT FINISHED
solve_hashi(Id) :-
    puzzle(Id, S, Islands),
    make_board(S, Islands, Board),
    hashi_board(Board).

make_board(S, Islands, Board) :-
    dim(Board, [S, S]),
    % max 8 bridges for one island (2 in each direction)
    Board #:: 0..8,
    (foreachindex([I,J], Board), param(Islands, Board) do
        (in_islands(Islands, (I, J, N)) -> 
            arg([I,J], Board, N)
        ;
            arg([I,J], Board, 0)
        )
    ),
    (for(I, 1, S), param(Board, S) do
        Ln is Board[I, 1..S],
        writeln(Ln)
    ).

% Checks if (X, Y, N) is contained in the Islands list
in_islands([], (_, _, _)) :- false.
in_islands([(X,Y,N)|_], (X, Y, N)).
in_islands([(_,_,_)|Rest], (X, Y, N)):-
    in_islands(Rest, (X, Y, N)).

test(Board) :-
    dim(Board, Size),
    element(1, Size, X),
    element(2, Size, Y),
    write(X),
    write(Y)
    .

get_island_amount(Islands, N) :-
    length(Islands, N).

get_random_island(Islands, (IslX, IslY, IslN)) :-
    random_element(Islands, (IslX, IslY, IslN)).

connected_constraints(Board, NESW, Islands) :-
    random_element(Islands, (SinkX, SinkY, _)),
    write(SinkX),nl,
    write(SinkY),nl,
    length(Islands, TempNb),
    % Needed for sink netflow comparison. 
    NbIslands is TempNb - 1,
    FlowCount is 0,
    dim(NESW, Dim),
    dim(Flow, Dim),
    Flow #:: -1.0Inf..1.0Inf, 
    dim(Board, Size),
    (multifor([I,J], [1,1], Size), param(NESW, Board, Flow, FlowCount, SinkX, SinkY, Size) do
        FlowN is Flow[I, J, 1],
        FlowE is Flow[I, J, 2],
        FlowS is Flow[I, J, 3],
        FlowW is Flow[I, J, 4],

        (arg([I,J], Board, 0) ->
            (arg([I, J], NESW, [](0,0,0,0)) ->
                % 1. If no island and no bridges -> flow is (0, 0, 0, 0).
                arg([I, J], Flow, [](0, 0, 0, 0))
            ;  
                % 3. If bridge cell has flow in direction, cell in that direction should have negative flow in opposite direction.
                ( I > 1 ->
                    ( FlowN #\= 0 *-> Flow[I-1, J, 3] #= -FlowN ; Flow[I-1, J, 3] #= 0)
                ;
                    true
                )
                ,
                % if I equals XSize (on border)
                ( element(1, Size, I) ->
                    true
                ;
                    ( FlowS #\= 0 *-> Flow[I+1, J, 1] #= -FlowS ; Flow[I+1, J, 1] #= 0)
                ),
                ( J > 1 ->
                    ( FlowW #\= 0 *-> Flow[I, J-1, 2] #= -FlowW ; Flow[I, J-1, 2] #= 0)
                ;
                    true
                ),
                % if J equals YSize (on border)
                ( element(2, Size, J) ->
                    true
                ;
                    ( FlowE #\= 0 *-> Flow[I, J+1, 4] #= -FlowE ; Flow[I, J+1, 4] #= 0)         
                ),
                % 2. If no island, the sum of flows on bridges = 0.
                FlowN + FlowE + FlowS + FlowW #= 0
            )
        ;
            % 4. Each non sink island adds one to FlowCount
            ( (I ~= SinkX ; J ~= SinkY) -> FlowCount + 1 ; true)
        )
    ),
    % 5. net flow at sink equals the amount of Islands - 1.
    FlowCount #= NbIslands,
    labeling(Flow)
    .


hashi_board(Board, Islands):- 
    dim(Board, [Imax,Jmax]),
    dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
    ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax) do
        Sum is Board[I,J],
        N is NESW[I,J,1],
        E is NESW[I,J,2],
        S is NESW[I,J,3],
        W is NESW[I,J,4],

        % Constraints 1 and 2:
        % The combination of N=S etc on tiles without island
        % and f.i. north on current tile must equal south on
        % other northern tile, express that bridges can only run
        % in a straight line, horizontally or vertically.
        % They also express another constraint that was not stated
        % explicitly: that bridges must be on both sides connected
        % to islands.
        ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
        ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
        ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
        ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
        % If we are on an island
        ( Sum > 0 ->
          % Constraint 4
          % Max 2 bridges connect
          [N,E,S,W] #:: 0..2,

          % Constraint 5
          % The sum of all bridges equals the island number
          N+E+S+W #= Sum
        ;
          % Not on an island
          N = S, 
          E = W,

          % Constraint 3
          % Bridges cannot cross each other (if not on an island)
          (N #= 0) or (E #= 0)
        )
    ),
    connected_constraints(Board, NESW, Islands),
    % find a solution
    labeling(NESW),
    print_board(Board, NESW).

hashi(Name) :-
        board(Name, Board),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax) do
            Sum is Board[I,J],
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],

            % Constraints 1 and 2:
            % The combination of N=S etc on tiles without island
            % and f.i. north on current tile must equal south on
            % other northern tile, express that bridges can only run
            % in a straight line, horizontally or vertically.
            % They also express another constraint that was not stated
            % explicitly: that bridges must be on both sides connected
            % to islands.
            ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
            ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
            ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
            ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
            % If we are on an island
            ( Sum > 0 ->
              % Constraint 4
              % Max 2 bridges connect
              [N,E,S,W] #:: 0..2,

              % Constraint 5
              % The sum of all bridges equals the island number
              N+E+S+W #= Sum
            ;
              % Not on an island
              N = S, 
              E = W,

              % Constraint 3
              % Bridges cannot cross each other (if not on an island)
              (N #= 0) or (E #= 0)
            )
        ),

        % find a solution
        labeling(NESW),
        print_board(Board, NESW).


print_board(Board, NESW) :-
        ( foreachindex([I,J],Board), param(Board,NESW) do
            ( J > 1 -> true ; nl ),
            Sum is Board[I,J],
            % if on an island
            ( Sum > 0 ->
                write(Sum)
            ;
            % if not on an island
                NS is NESW[I,J,1],
                EW is NESW[I,J,2],
                symbol(NS, EW, Char),
                write(Char)
            ),
            write(' ')
        ),
        nl.



% symbol( north-south bridge(s), east-west bridge(s), visualisation)
symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').



% Examples

board(stackoverflow,
     []([](4, 0, 6, 0, 0, 0, 6, 0, 3),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 1, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 1, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](1, 0, 3, 0, 0, 2, 0, 0, 0),
        [](0, 3, 0, 0, 0, 0, 4, 0, 1))
    ).
board(wikipedia,
     []([](2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1),
        [](0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0),
        [](2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0),
        [](0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0),
        [](1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0),
        [](0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2),
        [](0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3),
        [](0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0),
        [](0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2))
    ).

 puzzle(0, 4, P) :-
    P = [ (1,1,2), (1,4,2),
          (4,1,2), (4,4,2)].

% Example:
% connected_constraints([]([](2,0,0,2), [](0,0,0,0), [](0,0,0,0), [](2,0,0,2)), []([]([](0,1,1,0), [](0,1,0,1), [](0,1,0,1), [](0,0,1,1)),[]([](1,0,1,0), [](0,0,0,0), [](0,0,0,0), [](1,0,1,0)),[]([](1,0,1,0), [](0,0,0,0), [](0,0,0,0), [](1,0,1,0)),[]([](0,1,0,1), [](0,1,0,1), [](0,1,0,1), [](0,0,1,1))),[(1,1,2), (1,4,2), (4,1,2), (4,4,2)])