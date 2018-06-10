%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).


% Player List
playerList([a, b]).
otherPlayer(Player, Opponent) :-
	playerList(List),
	member(Opponent, List),
	Opponent \= Player.
index(0).
index(1).
index(2).
index(3).
index(4).
index(5).
index(6).


% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([], _, []).
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
elementOfRow(Row, Index, E) :-
	nth0(Index, Row, E).
extract_element(L, L1, [H|L1]):-
	length(L1, N1), length(L2, N1),
	append(L2, [H|_], L).
diagonal1(In, Out):-
	foldl(extract_element, In, [], Res),
	reverse(Res,Out).
diagonal2(In, Out):-
	reverse(In, In2),
	foldl(extract_element, In2, [], Out).
isBoardFull(Board) :-
	foreach(member(C, Board), \+member(0, C)).


checkVertical(Board, CNum, Player) :-
	nth0(CNum, Board, C),
	sublist([Player, Player, Player, Player], C).
checkHorizontal(Board, RNum, Player) :-
	transpose(Board, M), nth0(RNum, M, R),
	sublist([Player, Player, Player, Player], R).
checkDiagonal(Board, Player) :-
	diagonal1(Board, D),
	sublist([Player, Player, Player, Player], D).
checkDiagonal(Board, Player) :-
	diagonal2(Board, D),
	sublist([Player, Player, Player, Player], D).


% Index should start at 0 to check all columns and rows
checkWin(Board, Index, Player) :-
	checkVertical(Board, Index, Player) ;
	checkHorizontal(Board, Index, Player) ;
	checkDiagonal(Board, Player).
checkWin(Board, Index, Player) :-
	Index2 is Index + 1, Index2 < 7,
	checkWin(Board, Index2, Player).

opponentWin(Board, Player) :-
	otherPlayer(Player, Opponent),
	checkWin(Board, 0, Opponent).


% canForceWin(+Board, ?ColumnNumber)
canForceWin(Board, CNum, Player) :-
	checkWin(Board, 0, Player).
canForceWin(Board, CNum, Player) :-
	\+isBoardFull(Board),
	addPiece(Board, CNum, Player, ResultBoard),
	checkWin(ResultBoard, 0, Player).
canForceWin(Board, CNum, Player) :-
	\+isBoardFull(Board),
	addPiece(Board, CNum, Player, ResultBoard),
	index(NewCNum), otherPlayer(Player, Opponent),
	addPiece(ResultBoard, NewCNum, Opponent, ResultBoard2),
	\+opponentWin(ResultBoard2, Player),
	index(NewCNum2),
	canForceWin(ResultBoard2, NewCNum2, Player).
