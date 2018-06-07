%% FINAL PROJECT - CONNECT4 %%
name("Katie", "Shin", "ksw092").
name("Joshua", "Smith", "jas790").


% addPieceToColumn(+Column, +Player, -OutputColumn)
addPieceToColumn([E|C], Player, [E2|OC]) :- E = 0, E2 = Player.
addPieceToColumn([E|C], Player, [_|OC]) :- E != 0, addPieceToColumn(C, Player, OC).


% addPiece(+InputBoard, +ColumnNumber, +Player, -ResultBoard)
addPiece(InputBoard, Cnum, Player, ResultBoard) :- nth0(Cnum, InputBoard, C), nth0(Cnum, ResultBoard, D),
                                                   addPieceToColumn(C, Player, D).


% canWin(+Board, ?ColumnNumber)


% canForceWin(+Board, ?ColumnNumber)

