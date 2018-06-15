:- use_module(library(clpfd)).
:- use_module(library(tabling)).

%% Data and indices that are added to the database at runtime


%% how many pieces in a row give victory
streakGoal(4).

%% this is necessary since it allows us to run some index-based predicates backwards to
%% find columns. Also, defines a default search order implicitly
column(3).
column(2).
column(4).
column(1).
column(5).
column(0).
column(6).

row(0).
row(1).
row(2).
row(3).
row(4).
row(5).




% Convenience predicates for bulk adding lists of moves where the list of moves
% where the moves 1-indexed column numbers in the order played (alternating between P1 and P2 playing)

% addPieces(+Board, +Moves, -Result, +P1, +P1)
addPieces(Result, [], _, _, Result).
addPieces(Board, [C | Moves], P1, P2, Result) :- D is C-1, addPiece(Board, P1, D, Next), addPieces(Next, Moves, P2, P1, Result).

emptyBoard(Width, Height, Board) :- emptyColumn(Height, C), initList(Width, C, Board).
emptyColumn(Height, Column) :- initList(Height, 0, Column).

initList(0, _, []).
initList(Length, E, [E | List]) :- Length>=0, N is Length-1, initList(N, E, List).

%% drawBoard and drawRow used for debugging
drawBoard(B) :- transpose(B, T), reverse(T, T2), forall(member(R, T2), drawRow(R)).
drawRow([]) :- write("\n").
drawRow([X| R]) :- format("|~w|", [X]), drawRow(R).

p1(a).
p2(b).

otherPlayer(Player, Opponent) :- p1(Player), p2(Opponent).
otherPlayer(Player, Opponent) :- p1(Opponent), p2(Player).


%% addPiece(+InputBoard, +Player, +ColumnNumber, -ResultBoard)
addPiece([C|Rest], Player, 0, [D|Rest]) :-
	addPieceToColumn(C, Player, D).
addPiece([C|Board], Player, CNum, [C|Result]) :-
	\+ CNum = 0,
	D is CNum - 1,
	addPiece(Board, Player, D, Result).


%% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = 0,
	E2 = Player,
	C = OC.
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = E2,
	\+ E=0,
	addPieceToColumn(C, Player, OC).

%% element(+CNum, +RNum, +Board, -Element)
element(X, Y, M, E) :-
	column(X),
	nth0(X, M, C),
	row(Y),
	nth0(Y, C, E).


%% predicates for checking if any player has won

diag1(_, _, 0, _, _).
diag1(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	X2 is X + 1,
	Y2 is Y + 1,
	S2 is Streak - 1,
	diag1(X2, Y2, S2, Board, Player).

diag2(_, _, 0, _, _).
diag2(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	X2 is X - 1,
	Y2 is Y + 1,
	S2 is Streak - 1,
	diag2(X2, Y2, S2, Board, Player).

horiz(_, _, 0, _, _).
horiz(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	X2 is X,
	Y2 is Y + 1,
	S2 is Streak - 1,
	horiz(X2, Y2, S2, Board, Player).

vert(_, _, 0, _, _).
vert(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	X2 is X + 1,
	Y2 is Y,
	S2 is Streak - 1,
	vert(X2, Y2, S2, Board, Player).


%% isBoardFull(+Board)
isBoardFull(Board) :-
	foreach(member(C, Board), \+ member(0, C)).


%% checkStreak(?Pos, +Streak, +Board, +Player)
checkStreak([X, Y], Streak, Board, Player) :-
	checkStreak(X, Y, Streak, Board, Player).


%% checkStreak(?X, ?Y, +Streak, +Board, +Player)
checkStreak(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	(diag1(X, Y, Streak, Board, Player) ;
	 diag2(X, Y, Streak, Board, Player) ;
 	 horiz(X, Y, Streak, Board, Player) ;
	 vert(X, Y, Streak, Board, Player)).


%% countNInARow(+Board, +Player, +RNum, -NumInRow)
countNInARow(Board, Player, N, M) :-
	setof(P, checkStreak(P,N,Board,Player), S),
	length(S, M).
countNInARow(Board, Player, N, 0) :-
	\+ setof(P, checkStreak(P,N,Board,Player), _).

%%
%% Predicates for finding the search order
%%

%% produces a list of column numbers that specifies what order the algorithm should check for
%% optimal moves.
%% searchOrder(+Board, +Player, -searchOrderList)
searchOrder(Board, Player, L) :-
	findall([X, N], countOpportunities(Board, Player, X, N), Columns),
	sort(2, @>=, Columns, Sorted),
	getKeys(Sorted, L).

%% getKeys(+PairVector, -SingleVector)
getKeys([], []).
getKeys([[X, _] | R1], [X | R2]) :- getKeys(R1, R2).

%% Counts the number of opportunites (3-in-a-rows) created by adding a piece to the given column
%% countOpportunities(+Board, +CNum, +Player, -NumOpportunities)
countOpportunities(Board, Player, CNum, 999999999) :-
	column(CNum),
	addPiece(Board, Player, CNum, RB),
	checkWin(RB, Player).
countOpportunities(Board, Player, CNum, N) :-
	column(CNum),
	streakGoal(M),
	K is M - 1,
	addPiece(Board, Player, CNum, RB),
	countNInARow(RB, Player, K, N).

%% checkWin(+Board, +Player)
checkWin(Board, Player) :- streakGoal(N), checkStreak(_, _, N, Board, Player).

%%
%% Predicates for counting
%%

% finds how many spots are in board (so like... width*height basically)
% totalSpots(+Board, -Spots)
totalSpots(Board, Spots) :- maplist(length, Board, CLengths), foldl(add, CLengths, 0, Spots).
add(X, Y, Z) :- Z is X+Y.

emptySpots(Board, Spots) :-
	(setof([X, Y], element(X, Y, Board, 0), EmptySpots) ->
		length(EmptySpots, Spots) ;
		Spots=0
	).

nonemptySpots(Board, Spots) :- totalSpots(Board, T), emptySpots(Board, S), Spots is T-S.

%%
%% Hashes and Records
%%

% determines the maximum of the hash function
maxHash(65565).

% Computes a positive integer between 0 and maxHash using a method that is sufficiently
% jumble-y-ish in order to produce hash table keys for recorda/2
% hash(+Object, -Hash)
hash(Board, Hash) :- maplist(hash, Board, Hashes), foldl(combHash, Hashes, 0, Hash).
hash(0, 0).
hash(P, 11) :- p1(P).
hash(P, 23) :- p2(P).

% combines two hashes in a assymetric and jumble-y way
combHash(Hash1, Hash2, Hash) :- maxHash(Max), Hash is (Hash1 + Hash2*8191) rem Max.

:- dynamic(rec/2).
:- dynamic(recSize/1).
:- dynamic(score/2).

recSize(0).

% saves the score for a particular board
% recordScore(+Board, +Player, +Score)
/*recordScore(Board, Player, Score) :-
	p1(Player), hash(Board, Hash), recorda(Hash, score(Board, Score)).
recordScore(Board, Player, Score) :-
	p2(Player), TrueScore is -Score, hash(Board, Hash), recorda(Hash, score(Board, TrueScore)).*/

/*recordScore(Board, Player, Score) :-
	p1(Player), term_hash(Board, Hash), asserta(rec(Hash, score(Board, Score))),
	retract(recSize(S)), N is S+1, asserta(recSize(N)).
recordScore(Board, Player, Score) :-
	p2(Player), term_hash(Board, Hash), TrueScore is -Score, asserta(rec(Hash, score(Board, TrueScore))),
	retract(recSize(S)), N is S+1, asserta(recSize(N)).*/

/*recordScore(Board, Player, Score) :-
	p1(Player), asserta(score(Board, Score)).
recordScore(Board, Player, Score) :-
	p2(Player), TrueScore is -Score, asserta(score(Board, TrueScore)).*/

recordScore(Board, Player, Score, Trie) :-
	p1(Player), trie_update(Trie, Board, Score).
recordScore(Board, Player, Score, Trie) :-
	p2(Player), TrueScore is -Score, trie_update(Trie, Board, TrueScore).

% recordedScore(+Board, +Player, -Score)
/*recordedScore(Board, Player, Score) :-
	p1(Player), hash(Board, Hash), recorded(Hash, score(Board, Score)).
recordedScore(Board, Player, Score) :-
	p2(Player), hash(Board, Hash), recorded(Hash, score(Board, TrueScore)), Score is -TrueScore.*/

/*recordedScore(Board, Player, Score) :-
	p1(Player), term_hash(Board, Hash), rec(Hash, score(Board, Score)).
recordedScore(Board, Player, Score) :-
	p2(Player), term_hash(Board, Hash), rec(Hash, score(Board, TrueScore)), Score is -TrueScore.*/

recordedScore(Board, Player, Score, Trie) :-
	p1(Player), trie_lookup(Trie, Board, Score).
recordedScore(Board, Player, Score, Trie) :-
	p2(Player), trie_lookup(Trie, Board, TrueScore), Score is -TrueScore.


/*recordedScore(Board, Player, Score) :-
	p1(Player), score(Board, Score).
recordedScore(Board, Player, Score) :-
	p2(Player), score(Board, TrueScore), Score is -TrueScore.*/

score(Board, P, Score) :-
	emptySpots(Board, E), Alpha is -E-1, Beta is E+1,
	trie_new(Trie), score(Board, P, Score, Alpha, Beta, Trie).

scoreWeak(Board, P, Score) :-
		trie_new(Trie), score(Board, P, Score, 0, 1, Trie).

score(Board, P, Score, _, _, Trie) :- recordedScore(Board, P, Score, Trie), !.
score(Board, P, Score, _, _, _) :-
	checkWin(Board, P), emptySpots(Board, E), Score is E+1, !.
	%drawBoard(Board),
	%format("~w", [Score]),
	%write("\n").
score(Board, P1, Score, _, _, _) :-
	otherPlayer(P1, P2), checkWin(Board, P2), emptySpots(Board, E), Score is -E-1, !.
	%drawBoard(Board),
	%format("~w", [Score]),
	%write("\n").
score(Board, P1, Score, Alpha, Beta, Trie) :-
	%get data
	otherPlayer(P1, P2),
	emptySpots(Board, E),
	Beta2 is min(E+1, Beta),
	%determine search order
	searchOrder(Board, P1, L),
	%debug
	drawBoard(Board),
	write("\n"),
	%format("~w\n\n",[L]),
	%find the maximum score possible among all possible moves
	(L=[] ->
		Score=0 ;
		(maxScore(Board, P1, P2, Score, Alpha, Beta2, Trie, -999999, L))
	), !,
	%now, record the result
	ignore(recordScore(Board, P1, Score, Trie)).

maxScore(_, _, _, Score, Alpha, Beta, _, Max, _) :- Alpha >= Beta, Score is max(Beta, Max).
maxScore(_, _, _, Max, _, _, _, Max, []).
maxScore(Board, P1, P2, Score, Alpha, Beta, Trie, Max, [M | L]) :-
	P2Alpha is -Beta,
	P2Beta is -Alpha,
	%perform turn and get opponent score
	addPiece(Board, P1, M, NextBoard),
	score(NextBoard, P2, S1, P2Alpha, P2Beta, Trie),
	%adjust alpha if we can narrow the window
	TestScore is max(-S1, Max),
	Alpha2 is max(TestScore, Alpha),
	%if we've already found a score greater than beta, end, else try the rest of the list
	(TestScore >= Beta ->
		(Score is TestScore);
		maxScore(Board, P1, P2, Score, Alpha2, Beta, Trie, TestScore, L)
	).
