%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").


% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([], _, []).
addPieceToColumn([E|C], Player, [E2|OC]) :- E = 0, E2 = Player, C=OC.
addPieceToColumn([E|C], Player, [E2|OC]) :- E = E2, \+E=0, addPieceToColumn(C, Player, OC).


% addPiece(+InputBoard, +ColumnNumber, +Player, -ResultBoard)
addPiece([C|Rest], 0, Player, [D|Rest]) :- addPieceToColumn(C, Player, D).
addPiece([C|Board], CNum, Player, [C|Result]) :- \+CNum=0, D is CNum-1, addPiece(Board, D, Player, Result).


% canWin(+Board, ?ColumnNumber)


% canForceWin(+Board, ?ColumnNumber)

