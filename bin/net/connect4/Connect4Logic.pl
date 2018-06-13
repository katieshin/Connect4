%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).
:- use_module(library(tabling)).
:- use_module(library(assoc)).

% hard-coded values
streakGoal(3).

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

emptyBoard([[0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0,0,0,0,0,0]]).


% Player List
playerList([a, b]).
otherPlayer(Player, Opponent) :-
	playerList(List),
	member(Opponent, List),
	Opponent \= Player.


% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = 0, E2 = Player, C=OC.
addPieceToColumn([E|C], Player, [E2|OC]) :-
	E = E2,
	\+E=0, addPieceToColumn(C, Player, OC).


% addPiece(+InputBoard, +ColumnNumber, +Player, -ResultBoard)
addPiece([C|Rest], 0, Player, [D|Rest]) :-
	addPieceToColumn(C, Player, D).
addPiece([C|Board], CNum, Player, [C|Result]) :-
	\+CNum=0,
	D is CNum-1,
	addPiece(Board, D, Player, Result).

addPieces(Moves, EndBoard, P1, P2) :- emptyBoard(B), addPieces(B, Moves, EndBoard, P1, P2).
addPieces(Board, [], Board, _, _).
addPieces(Board, [C | Moves], EndBoard, P1, P2) :- D is C-1, addPiece(Board, D, P1, Next), addPieces(Next, Moves, EndBoard, P2, P1).


element(X, Y, M, E) :- column(X), nth0(X, M, C), row(Y), nth0(Y, C, E).

drawBoard(B) :- transpose(B, T), reverse(T, T2), forall(member(R, T2), drawRow(R)).
drawRow([]) :- write("\n").
drawRow([X| R]) :- format("|~w|", [X]), drawRow(R).



% predicates for checking if any player has won
sublist(Sublist, List) :-
	append([_, Sublist, _], List).

diag1(_, _, 0, _, _).
diag1(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X+1, Y2 is Y+1, S2 is Streak-1, diag1(X2, Y2, S2, Board, Player).

diag2(_, _, 0, _, _).
diag2(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X-1, Y2 is Y+1, S2 is Streak-1, diag2(X2, Y2, S2, Board, Player).

horiz(_, _, 0, _, _).
horiz(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X, Y2 is Y+1, S2 is Streak-1, horiz(X2, Y2, S2, Board, Player).

vert(_, _, 0, _, _).
vert(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X+1, Y2 is Y, S2 is Streak-1, vert(X2, Y2, S2, Board, Player).

% isBoardFull(+Board)
isBoardFull(Board) :-
	foreach(member(C, Board), \+member(0, C)).

% checkStreak(?Pos, +Streak, +Board, +Player)
checkStreak([X, Y], Streak, Board, Player) :- checkStreak(X, Y, Streak, Board, Player).

% checkStreak(?X, ?Y, +Streak, +Board, +Player)
checkStreak(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player),
	(diag1(X, Y, Streak, Board, Player) ;
	diag2(X, Y, Streak, Board, Player) ;
	horiz(X, Y, Streak, Board, Player) ;
	vert(X, Y, Streak, Board, Player)).

countNInARow(Board, Player, N, M) :- setof(P, checkStreak(P,N,Board,Player), S), length(S, M).
countNInARow(Board, Player, N, 0) :- \+ setof(P, checkStreak(P,N,Board,Player), _).

searchOrder(Board, Player, L) :-
	findall([X, N], countOpportunities(Board, X, Player, N), Columns), sort(2, @>=, Columns, Sorted), getKeys(Sorted, L).

countOpportunities(Board, CNum, Player, 999999999) :-
	column(CNum), addPiece(Board, CNum, Player, RB), checkWin(RB, Player).
countOpportunities(Board, CNum, Player, N) :-
	streakGoal(M), K is M-1,
	column(CNum), addPiece(Board, CNum, Player, RB), countNInARow(RB, Player, K, N).

getKeys([], []).
getKeys([[X, _] | R1], [X | R2]) :- getKeys(R1, R2).


%:- table checkWin/2.

% column should start at 0 to check all columns and rows
% checkWin(+Board, +Player)
checkWin(Board, Player) :- streakGoal(N), checkStreak(_, _, N, Board, Player).

%:- table canForceWin(_, first, _).

% canForceWin(+Board, ?ColumnNumber, +Player)
canForceWin(Board, CNum, Player) :-
	%drawBoard(Board), write("\n"),
	searchOrder(Board, Player, L),
	%format("~w\n\n", [L]),
	searchForBestMove(Board, CNum, Player, L).

searchForBestMove(_, CNum, _, []) :- !, column(CNum).
searchForBestMove(Board, CNum, Player, Order) :-
	member(CNum, Order),
	%format("~w is moving at ~w\n", [Player, CNum]),
	addPiece(Board, CNum, Player, ResultBoard),
	%drawBoard(ResultBoard), write("\n"),
	madeOptimumMove(ResultBoard, Player), !.%, write("Success!\n"), !.

:- dynamic memo/1.
memo(A) :- empty_assoc(A).

%:- table madeOptimumMove/2.
%madeOptimumMove(RB, Player) :- memo(A), get_assoc([RB, Player], A, true).
%madeOptimumMove(RB, Player) :- memo(A), get_assoc([RB, Player], A, false), !, fail.
madeOptimumMove(RB, Player) :- checkWin(RB, Player).
	%,ignore(updateMemo(RB, Player, true)).
madeOptimumMove(RB, Player) :- isBoardFull(RB).
	%,ignore(updateMemo(RB, Player, true)).
madeOptimumMove(RB, Player) :-
	(checkWin(RB, Player) ;
	isBoardFull(RB);
	(\+ canOpponentImmediatelyWin(RB, Player),
	otherPlayer(Player, Opponent), searchOrder(RB, Opponent, L),
	forall(member(CNum, L), canBeatOpponent(RB, CNum, Player)))).
	%,ignore(updateMemo(RB, Player, true)).
%madeOptimumMove(RB, Player) :- ignore(updateMemo(RB, Player, false)), !, fail.

updateMemo(RB, Player, Value) :-
	memo(A), put_assoc([RB, Player], A, Value, A2), ignore(retractall(memo(_))), ignore(asserta(memo(A2))).

canOpponentImmediatelyWin(RB, Player) :-
	column(CNum), otherPlayer(Player, Opponent), addPiece(RB, CNum, Opponent, RB2), checkWin(RB2, Opponent).

canBeatOpponent(ResultBoard, CNum2, _) :- nth0(CNum2, ResultBoard, C), \+ member(0, C).
canBeatOpponent(ResultBoard, CNum2, Player) :-
	otherPlayer(Player, Opponent), addPiece(ResultBoard, CNum2, Opponent, RB2),
	%format("~w is moving at ~w\n", [Opponent, CNum2]),
	canForceWin(RB2, _, Player).