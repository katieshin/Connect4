%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").

:- use_module(library(clpfd)).

% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([], _, []).
addPieceToColumn([E|C], Player, [E2|OC]) :- E = 0, E2 = Player, C=OC.
addPieceToColumn([E|C], Player, [E2|OC]) :- E = E2, \+E=0, addPieceToColumn(C, Player, OC).


% addPiece(+InputBoard, +ColumnNumber, +Player, -ResultBoard)
addPiece([C|Rest], 0, Player, [D|Rest]) :- addPieceToColumn(C, Player, D).
addPiece([C|Board], CNum, Player, [C|Result]) :- \+CNum=0, D is CNum-1, addPiece(Board, D, Player, Result).


% canWin(+Board, ?ColumnNumber)
sublist(Sublist, List) :- append([_, Sublist, _], List).
elementOfRow(Row, Index, E) :- nth0(Index, Row, E).
extract_element(L, L1, [H|L1]):- length(L1, N1), length(L2, N1), 
               					 append(L2, [H|_], L).
diagonal1(In, Out):- foldl(extract_element, In, [], Res), 
                	 reverse(Res,Out).
diagonal2(In, Out):- reverse(In, In2),
                	 foldl(extract_element, In2, [], Out).
checkFullBoard(Board) :- foreach(member(C, Board),
								 \+member(0, C)).



checkVertical(Board, CNum, Player) :- nth0(CNum, Board, C),
									  sublist([Player, Player, Player, Player], C).
checkHorizontal(Board, RNum, Player) :- transpose(Board, M), nth0(RNum, M, R),
										sublist([Player, Player, Player, Player], R).
checkDiagonal(Board, Player) :- diagonal1(Board, D),
								sublist([Player, Player, Player, Player], D).
checkDiagonal(Board, Player) :- diagonal2(Board, D),
								sublist([Player, Player, Player, Player], D).


% canForceWin(+Board, ?ColumnNumber)

