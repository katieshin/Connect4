%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).
:- use_module(library(assoc)).

:- dynamic memo/1.
memo(A) :- empty_assoc(A).

%% streakGoal will be defined in the Java code
% streakGoal(3).

%% numbers of columns and rows will be defined in the Java code
/*
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
*/

%% addPieces added for testing, and emptyBoard size will be determined by Java code
/*
addPieces(Moves, EndBoard, P1, P2) :- emptyBoard(B), addPieces(B, Moves, EndBoard, P1, P2).
addPieces(Board, [], Board, _, _).
addPieces(Board, [C | Moves], EndBoard, P1, P2) :- D is C-1, addPiece(Board, D, P1, Next), addPieces(Next, Moves, EndBoard, P2, P1).

emptyBoard([[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0]]).
*/

%% drawBoard and drawRow used for debugging
/*
drawBoard(B) :- transpose(B, T), reverse(T, T2), forall(member(R, T2), drawRow(R)).
drawRow([]) :- write("\n").
drawRow([X| R]) :- format("|~w|", [X]), drawRow(R).
*/


%% Player List
playerList([a, b]).
otherPlayer(Player, Opponent) :-
	playerList(List),
	member(Opponent, List),
	Opponent \= Player.


%% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = 0,
	E2 = Player,
	C = OC.
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = E2,
	\+ E=0,
	addPieceToColumn(C, Player, OC).


%% addPiece(+InputBoard, +ColumnNumber, +Player, -ResultBoard)
addPiece([C|Rest], 0, Player, [D|Rest]) :-
	addPieceToColumn(C, Player, D).
addPiece([C|Board], CNum, Player, [C|Result]) :-
	\+ CNum = 0,
	D is CNum - 1,
	addPiece(Board, D, Player, Result).


%% element(+CNum, +RNum, +Board, -Element)
element(X, Y, M, E) :-
	column(X),
	nth0(X, M, C),
	row(Y),
	nth0(Y, C, E).


%% predicates for checking if any player has won
sublist(Sublist, List) :-
	append([_, Sublist, _], List).

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


%% searchOrder(+Board, +Player, -searchOrderList)
searchOrder(Board, Player, L) :-
	findall([X, N], countOpportunities(Board, X, Player, N), Columns),
	sort(2, @>=, Columns, Sorted),
	getKeys(Sorted, L).


%% countOpportunities(+Board, +CNum, +Player, -NumOpportunities)
countOpportunities(Board, CNum, Player, 999999999) :-
	column(CNum),
	addPiece(Board, CNum, Player, RB),
	checkWin(RB, Player).
countOpportunities(Board, CNum, Player, N) :-
	streakGoal(M),
	K is M - 1,
	column(CNum),
	addPiece(Board, CNum, Player, RB),
	countNInARow(RB, Player, K, N).


%% getKeys(+PairVector, -SingleVector)
getKeys([], []).
getKeys([[X, _] | R1], [X | R2]) :- getKeys(R1, R2).


%% checkWin(+Board, +Player)
checkWin(Board, Player) :- streakGoal(N), checkStreak(_, _, N, Board, Player).


%% canForceWin(+Board, ?ColumnNumber, +Player)
canForceWin(Board, CNum, Player) :-
	searchOrder(Board, Player, L),
	searchForBestMove(Board, CNum, Player, L).


%% searchForBestMove(+Board, +CNum, +Player, -OrderList)
searchForBestMove(_, CNum, _, []) :-
	!, column(CNum).
searchForBestMove(Board, CNum, Player, Order) :-
	member(CNum, Order),,
	addPiece(Board, CNum, Player, ResultBoard),
	madeOptimumMove(ResultBoard, Player), !.


%% madeOptimumMove(+Board, +Player)
madeOptimumMove(Board, Player) :-
	checkWin(Board, Player).
madeOptimumMove(Board, Player) :-
	isBoardFull(Board).
madeOptimumMove(Board, Player) :-
	(checkWin(Board, Player) ;
	 isBoardFull(Board);
	(\+ canOpponentImmediatelyWin(Board, Player),
	 otherPlayer(Player, Opponent),
	 searchOrder(Board, Opponent, L),
	 forall(member(CNum, L), canBeatOpponent(Board, CNum, Player)))).


%% updateMemo(+Board, +Player, +NewValue)
updateMemo(Board, Player, Value) :-
	memo(A),
	put_assoc([Board, Player], A, Value, A2),
	ignore(retractall(memo(_))),
	ignore(asserta(memo(A2))).


%% canOpponentImmediatelyWin(+Board, +Player)
canOpponentImmediatelyWin(Board, Player) :-
	column(CNum),
	otherPlayer(Player, Opponent),
	addPiece(Board, CNum, Opponent, RB),
	checkWin(RB, Opponent).


%% canBeatOpponent(+Board, +CNum, +Player)
canBeatOpponent(Board, CNum, _) :-
	nth0(CNum, Board, C),
	\+ member(0, C).
canBeatOpponent(Board, CNum, Player) :-
	otherPlayer(Player, Opponent),
	addPiece(Board, CNum, Opponent, RB),
	canForceWin(RB, _, Player).
