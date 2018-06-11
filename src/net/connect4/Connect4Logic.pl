%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).


% hard-coded values
index(3).
index(2).
index(4).
index(1).
index(5).
index(0).
index(6).

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


% predicates for checking if any player has won
sublist(Sublist, List) :-
	append([_, Sublist, _], List).

diag1(Board, Player) :-
	diag1Col(CNum1), diag1Row(RNum1),
	CNum2 is CNum1 + 1, CNum3 is CNum1 + 2, CNum4 is CNum1 + 3,
	RNum2 is RNum1 + 1, RNum3 is RNum1 + 2, RNum4 is RNum1 + 3,
	nth0(CNum1, Board, Col1),
	nth0(CNum2, Board, Col2),
	nth0(CNum3, Board, Col3),
	nth0(CNum4, Board, Col4),
	nth0(RNum1, Col1, Player),
	nth0(RNum2, Col2, Player),
	nth0(RNum3, Col3, Player),
	nth0(RNum4, Col4, Player).

diag2(Board, Player) :-
	diag2Col(CNum1), diag2Row(RNum1),
	CNum2 is CNum1 - 1, CNum3 is CNum1 - 2, CNum4 is CNum1 - 3,
	RNum2 is RNum1 + 1, RNum3 is RNum1 + 2, RNum4 is RNum1 + 3,
	nth0(CNum1, Board, Col1),
	nth0(CNum2, Board, Col2),
	nth0(CNum3, Board, Col3),
	nth0(CNum4, Board, Col4),
	nth0(RNum1, Col1, Player),
	nth0(RNum2, Col2, Player),
	nth0(RNum3, Col3, Player),
	nth0(RNum4, Col4, Player).

isBoardFull(Board) :-
	foreach(member(C, Board), \+member(0, C)).


checkVertical(Board, CNum, Player) :-
	nth0(CNum, Board, C),
	sublist([Player, Player, Player, Player], C).
checkHorizontal(Board, RNum, Player) :-
	transpose(Board, M), nth0(RNum, M, R),
	sublist([Player, Player, Player, Player], R).
checkDiagonal(Board, Player) :-
	diag1(Board, Player) ;
	diag2(Board, Player).


% Index should start at 0 to check all columns and rows
checkWin(Board, Player) :-
	checkDiagonal(Board, Player) ; 
	(index(Index),
	(checkVertical(Board, Index, Player) ;
	checkHorizontal(Board, Index, Player))).

opponentWin(Board, Player) :-
	otherPlayer(Player, Opponent),
	checkWin(Board, Opponent).


% canForceWin(+Board, ?ColumnNumber)
canForceWin(Board, _, Player) :- otherPlayer(Player, Opponent), \+ checkWin(Board, Opponent), isBoardFull(Board).
canForceWin(Board, CNum, Player) :-
	index(CNum),
	addPiece(Board, CNum, Player, ResultBoard),
	checkWin(ResultBoard, Player).
canForceWin(Board, CNum, Player) :-
	format("~w\n", [Board]),	
	index(CNum),
	addPiece(Board, CNum, Player, ResultBoard),
	forall(index(CNum2), helper(ResultBoard, CNum2, Player)).

helper(ResultBoard, CNum2, Player) :-	
	otherPlayer(Player, Opponent), addPiece(ResultBoard, CNum2, Opponent, RB2), \+ checkWin(RB2, Opponent), canForceWin(RB2, _, Player).


