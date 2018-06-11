%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).


% hard-coded values
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


diag1Col(0).
diag1Col(1).
diag1Col(2).
diag1Col(3).
diag1Row(0).
diag1Row(1).
diag1Row(2).

diag2Col(3).
diag2Col(4).
diag2Col(5).
diag2Col(6).
diag2Row(0).
diag2Row(1).
diag2Row(2).


emptyBoard([[0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0,0,0,0,0,0]]).

% Player List
playerList(["*", "#"]).
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


% predicates for checking if any player has won
sublist(Sublist, List) :-
	append([_, Sublist, _], List).

diag1(_, _, 0, _, _). 
diag1(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X+1, Y2 is Y+1, S2 is Streak-1, diag1(X2, Y2, S2, Board, Player).

diag2(_, _, 0, _, _). 
diag2(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X+1, Y2 is Y-1, S2 is Streak-1, diag2(X2, Y2, S2, Board, Player).

horiz(_, _, 0, _, _). 
horiz(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X+1, Y2 is Y, S2 is Streak-1, horiz(X2, Y2, S2, Board, Player).

vert(_, _, 0, _, _). 
vert(X, Y, Streak, Board, Player) :-
	element(X, Y, Board, Player), X2 is X, Y2 is Y+1, S2 is Streak-1, vert(X2, Y2, S2, Board, Player).

% isBoardFull(+Board)
isBoardFull(Board) :-
	foreach(member(C, Board), \+member(0, C)).

% checkStreak(?Pos, +Streak, +Board, +Player)
checkStreak([X, Y], Streak, Board, Player) :- checkStreak(X, Y, Streak, Board, Player).

% checkStreak(?X, ?Y, +Streak, +Board, +Player)
checkStreak(X, Y, Streak, Board, Player) :- 
	(diag1(X, Y, Streak, Board, Player) ; 
	diag2(X, Y, Streak, Board, Player) ; 
	horiz(X, Y, Streak, Board, Player) ; 
	vert(X, Y, Streak, Board, Player)).

countThreeInARow(Board, Player, N) :- setof(P, checkStreak(P,3,Board,Player), S), length(S, N).


% column should start at 0 to check all columns and rows
% checkWin(+Board, +Player)
checkWin(Board, Player) :-
	element(X, Y, Board, Player), checkStreak(X, Y, 4, Board, Player).


% canForceWin(+Board, ?ColumnNumber, +Player)
canForceWin(Board, _, Player) :- otherPlayer(Player, Opponent), \+ checkWin(Board, Opponent), isBoardFull(Board).
canForceWin(Board, CNum, Player) :-
	column(CNum),
	addPiece(Board, CNum, Player, ResultBoard),
	checkWin(ResultBoard, Player).
canForceWin(Board, CNum, Player) :-
	format("~w\n", [Board]),	
	column(CNum),
	addPiece(Board, CNum, Player, ResultBoard),
	forall(column(CNum2), helper(ResultBoard, CNum2, Player)).

helper(ResultBoard, CNum2, _) :- nth0(CNum2, ResultBoard, C), \+ member(0, C).
helper(ResultBoard, CNum2, Player) :-	
	otherPlayer(Player, Opponent), addPiece(ResultBoard, CNum2, Opponent, RB2), \+ checkWin(RB2, Opponent), canForceWin(RB2, _, Player).


