:- consult(utils).

% display(+Piece, -Display)
% Defines display for each of the pieces of the board

display(null, '    ').
display(empty,' ## ').
display(rock1,' R1 ').
display(rock2,' R2 ').
display(rock3,' R3 ').
display(rock4,' R4 ').
display(stonetroll1,' T1 ').
display(stonetroll2,' T2 ').
display(dwarf1,' D1 ').
display(dwarf2,' D2 ').
display(sorcerer1,' S1 ').
display(sorcerer2,' S2 ').

% get_piece(+Player, ?Piece, ?BoardPiece)
% Defines relation betweem Player, Piece and the name of the BoardPiece

get_piece(0, 1, sorcerer1).
get_piece(0, 2, dwarf1).
get_piece(0, 3, stonetroll1).
get_piece(1, 1, sorcerer2).
get_piece(1, 2, dwarf2).
get_piece(1, 3, stonetroll2).
get_piece(_, 4, 0).
get_piece(_, 5, rock1).
get_piece(_, 6, rock2).
get_piece(_, 7, rock3).
get_piece(_, 8, rock4).

% initial_state(+Height, +Width, +RowNumber, -Board)
% Defines initial state of the board

initial_state(0, _, _, []).
initial_state(Height, Width, RowNumber, [Row | Rest]):-
    Height > 0,
    create_row(RowNumber, Width, Row),
    NewHeight is Height - 1,
    NewRowNumber is RowNumber + 1,
    initial_state(NewHeight, Width, NewRowNumber, Rest).

% create_row(+RowNumber, +Width, -Row)
% Creates Row with the correct size

create_row(RowNumber, Width, Row) :-
    length(Row, Width),
    fill_row(RowNumber, Row, Width).

% fill_row(+RowNumber, -Row, +Width)
% Fills the Row with the correct pieces

fill_row(0, Row, Width) :-
    Coordinates is (Width + 1) // 2,
    nth1(Coordinates, Row, rock1).
fill_row(1, Row, Width) :-
    Coordinates1 is Width // 2,
    Coordinates2 is Coordinates1 + 1,
    Coordinates3 is Coordinates2 + 1,
    nth1(Coordinates1, Row, stonetroll1),
    nth1(Coordinates2, Row, dwarf1),
    nth1(Coordinates3, Row, sorcerer1).
fill_row(RowNumber, Row, Width) :-
    RowNumber =:= (Width - 1),
    Coordinates is (Width + 1) // 2,
    nth1(Coordinates, Row, rock2).
fill_row(RowNumber, Row, Width) :-
    RowNumber =:= Width - 2,
    Coordinates1 is Width // 2,
    Coordinates2 is Coordinates1 + 1,
    Coordinates3 is Coordinates2 + 1,
    nth1(Coordinates1, Row, sorcerer2),
    nth1(Coordinates2, Row, dwarf2),
    nth1(Coordinates3, Row, stonetroll2).  
fill_row(RowNumber, Row, Width) :-
    RowNumber =:= (Width//2),
    nth1(1, Row, rock3),
    nth1(Width, Row, rock4),
    NumberEmpty is Width - 2,
    fill_empty(2, NumberEmpty, Row).
fill_row(RowNumber, Row, Width):-
    RowNumber >= 2,
    RowNumber < (Width // 2),
    Coordinates is (Width - (RowNumber*2) + 1) // 2,
    NumberEmpty is (RowNumber*2) + 1,
    fill_empty(Coordinates, NumberEmpty, Row).
fill_row(RowNumber, Row, Width):-
    RowNumber > Width // 2,
    RowNumber < (Width - 2),
    Middle is Width // 2,
    Difference is RowNumber - Middle,
    NumberEmpty is ((Middle - Difference) * 2) + 1,
    Coordinates is ((Width - NumberEmpty) // 2) + 1,
    fill_empty(Coordinates, NumberEmpty, Row).

% fill_empty(+Initial, +NumberEmpty, -Row)
% Fills with empty in the correct places of each Row
fill_empty(_, 0, _).
fill_empty(Initial, NumberEmpty, Row):-
    NumberEmpty > 0,
    nth1(Initial, Row, empty),
    NewInitial is Initial + 1,
    NewNumberEmpty is NumberEmpty - 1,
    fill_empty(NewInitial, NewNumberEmpty, Row).

% get_symbol(+Board,+Line,+Col,-Symbol)
% Gets symbol in the given coordinates
get_symbol(Board,Line,Col,Symbol):-
    nth0(Line, Board, LineList),
    nth0(Col,LineList,Symbol).

% display_line(+Board,+Line,+Col,+Size)
% Display Line of Board

display_line(_,_,Col,Size):-
    Col >= Size,
    nl, nl.
display_line(Board,Line,Col,Size):-
    get_symbol(Board,Line,Col,Symbol),
    display(Symbol,Display),
    write(Display),
    Newcol is Col + 1,
    display_line(Board, Line, Newcol, Size).

% display_game(+Board,+Line,+Size, +State)
% Display of the game in the current State

display_game(_,Line,Size,0):-
    Line >= Size.
display_game(_,Line,Size,1):-
    Line >= Size,
    write('------------------------------'),
    write('PLAYER 1 WON'),
    write('------------------------------').
display_game(_,Line,Size,2):-
    Line >= Size,
    write('------------------------------'),
    write('PLAYER 2 WON '),
    write('------------------------------').
display_game(Board, Line, Size,State):-
    display_line(Board,Line,0,Size),
    Newline is Line + 1 ,
    display_game(Board, Newline, Size,State).


% find_piece_board(+Piece, +Board, +NumberRow, -PositionPiece, +BoardSize)
% Gets position of a specific piece in the board

find_piece_board(0, _, _, _, _):-!.
find_piece_board(Piece, Board, NumberRow, (NumberRow, FinalColumn), BoardSize):-
    NumberRow < BoardSize,
    nth0(NumberRow, Board, Row),
    find_piece_row(Piece, Row, 0, FinalColumn, BoardSize),!.
find_piece_board(Piece, Board, NumberRow, (FinalRow, FinalColumn), BoardSize):-
    NumberRow < BoardSize,
    NewNumberRow is NumberRow + 1,
    find_piece_board(Piece, Board, NewNumberRow, (FinalRow, FinalColumn), BoardSize),!.
find_piece_board(_, _, _, _, _):-!.
find_piece_row(Piece, Row, CurrentColumn, CurrentColumn, BoardSize):-
    CurrentColumn < BoardSize,
    nth0(CurrentColumn, Row, Piece).
find_piece_row(Piece, Row, CurrentColumn, FinalColumn, BoardSize):-
    CurrentColumn < BoardSize,
    NewNumberColumn is CurrentColumn + 1,
    find_piece_row(Piece, Row, NewNumberColumn, FinalColumn, BoardSize).


% change_rock_board(+Board, +OldPosition, +BoardSize, +NewPosition, +Rock, -FinalBoard,+Move,+Direction,+State)
% Changes the position of the rock in the board

change_rock_board(Board, _, _, _, 0, Board,Move,_,0):-
    Move < 5,!.
change_rock_board(Board, (Row, Column), BoardSize, (_, _), _, FinalBoard ,Move,Direction,State):-
    Move > 4, 
    throw_rock(Board,BoardSize,(Row, Column),Direction, Move, MidFinalBoard),
    game_over(MidFinalBoard,Row,Column,Direction,Move,State,BoardSize, FinalBoard), !.
change_rock_board(Board, (Row, Column), BoardSize, (NewRow, NewColumn), Rock, FinalBoard,_,_,0):-
    NewRow >= 0,
    NewRow < BoardSize,
    NewColumn >= 0,
    NewColumn < BoardSize,  
    nth0(NewRow, Board, CheckOldLine),
    nth0(NewColumn, CheckOldLine, CurrentElement),
    change_rock_position(Board, (Row, Column), BoardSize, (NewRow, NewColumn), Rock, FinalBoard,CurrentElement),!.

change_rock_board(Board, _, _, _, _, Board,_,_,0):-!.

change_rock_position(Board, (Row, Column), _, (NewRow, NewColumn), Rock, FinalBoard,empty):-
    nth0(Row, Board, OldLine),
    replace_element(Column, empty, OldLine, OldNewLine),
    replace_element(Row, OldNewLine, Board, NewBoard),
    nth0(NewRow, NewBoard, NextLine),
    nth0(NewColumn, NextLine, empty),
    replace_element(NewColumn, Rock, NextLine, NewLine),
    replace_element(NewRow, NewLine, NewBoard, FinalBoard),!.
change_rock_position(Board, (_, _), _, (_, _), _, Board,_):-!.

% change_board(+Board, +PositionPiece, +BoardSize, +Move, +BoardPiece, -NewBoard, +Direction, -RockPosition)
% Changes board after moving piece


change_board(Board, (Row, Column), BoardSize, 1, BoardPiece, FinalBoard,_,_):-
    is_dwarf(BoardPiece),
    NewRow is Row - 1,
    RockRow is NewRow - 1,
    NewRow >= 0,
    NewRow < BoardSize,
    nth0(NewRow, Board, RockLine),
    nth0(Column, RockLine, Symbol),
    is_rock(Symbol),
    nth0(RockRow, Board, NewRockLine),
    replace_element(Column, Symbol, NewRockLine, NewLine),
    replace_element(RockRow, NewLine, Board, Board2),
    nth0(NewRow, Board, DwarfLine),
    replace_element(Column, BoardPiece, DwarfLine, NewLine2),
    replace_element(NewRow, NewLine2, Board2, Board3),
    nth0(Row, Board, EmptyLine),
    replace_element(Column, empty, EmptyLine, NewLine3),
    replace_element(Row, NewLine3, Board3, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 2, BoardPiece, FinalBoard,_,_):-
    is_dwarf(BoardPiece),
    NewRow is Row + 1,
    RockRow is NewRow + 1,
    NewRow >= 0,
    NewRow < BoardSize,
    nth0(NewRow, Board, RockLine),
    nth0(Column, RockLine, Symbol),
    is_rock(Symbol),
    nth0(RockRow, Board, NewRockLine),
    replace_element(Column, Symbol, NewRockLine, NewLine),
    replace_element(RockRow, NewLine, Board, Board2),
    nth0(NewRow, Board, DwarfLine),
    replace_element(Column, BoardPiece, DwarfLine, NewLine2),
    replace_element(NewRow, NewLine2, Board2, Board3),
    nth0(Row, Board, EmptyLine),
    replace_element(Column, empty, EmptyLine, NewLine3),
    replace_element(Row, NewLine3, Board3, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 3, BoardPiece, FinalBoard,_,_):-
    is_dwarf(BoardPiece),
    NewCol is Column - 1,
    RockCol is NewCol - 1,
    NewCol >= 0,
    NewCol < BoardSize,
    nth0(Row, Board, RockLine),
    nth0(NewCol, RockLine, Symbol),
    is_rock(Symbol),
    nth0(Row, Board, NewRockLine),
    replace_element(RockCol, Symbol, NewRockLine, NewLine),
    replace_element(NewCol, BoardPiece, NewLine, NewLine2),
    replace_element(Column, empty, NewLine2, NewLine3),
    replace_element(Row, NewLine3, Board, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 4, BoardPiece, FinalBoard,_,_):-
    is_dwarf(BoardPiece),
    NewCol is Column + 1,
    RockCol is NewCol + 1,
    NewCol >= 0,
    NewCol < BoardSize,
    nth0(Row, Board, RockLine),
    nth0(NewCol, RockLine, Symbol),
    is_rock(Symbol),
    nth0(Row, Board, NewRockLine),
    replace_element(RockCol, Symbol, NewRockLine, NewLine),
    replace_element(NewCol, BoardPiece, NewLine, NewLine2),
    replace_element(Column, empty, NewLine2, NewLine3),
    replace_element(Row, NewLine3, Board, FinalBoard).
change_board(Board, (Row, Column), BoardSize, 1, Piece, FinalBoard,_,_):-
    NewRow is Row - 1,
    NewRow >= 0,
    NewRow < BoardSize,
    nth0(NewRow, Board, NextLine),
    nth0(Column, NextLine, empty),
    nth0(Row, Board, OldLine),
    replace_element(Column, Piece, NextLine, NewLine),
    replace_element(Column, empty, OldLine, OldNewLine),
    replace_element(NewRow, NewLine, Board, NewBoard),
    replace_element(Row, OldNewLine, NewBoard, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 2, Piece, FinalBoard,_,_):-
    NewRow is Row + 1,
    NewRow >= 0,
    NewRow < BoardSize,
    nth0(NewRow, Board, NextLine),
    nth0(Column, NextLine, empty),
    nth0(Row, Board, OldLine),
    replace_element(Column, Piece, NextLine, NewLine),
    replace_element(Column, empty, OldLine, OldNewLine),
    replace_element(NewRow, NewLine, Board, NewBoard),
    replace_element(Row, OldNewLine, NewBoard, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 3, Piece, FinalBoard,_,_):-
    NewCol is Column - 1,
    NewCol >= 0,
    NewCol < BoardSize,
    nth0(Row, Board, NextLine),
    nth0(NewCol, NextLine, empty),
    replace_element(NewCol, Piece, NextLine, NewLine),
    replace_element(Column, empty, NewLine, FinalLine),
    replace_element(Row, FinalLine, Board, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 4, Piece, FinalBoard,_,_):-
    NewCol is Column + 1,
    NewCol >= 0,
    NewCol < BoardSize,
    nth0(Row, Board, NextLine),
    nth0(NewCol, NextLine, empty),
    replace_element(NewCol, Piece, NextLine, NewLine),
    replace_element(Column, empty, NewLine, FinalLine),
    replace_element(Row, FinalLine, Board, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 5, Piece, FinalBoard, _,(NewRow,NewCol)):-
    find_piece_board(rock1, Board, 0, (NewRow,NewCol), BoardSize),
    NewCol >= 0,
    NewCol < BoardSize,
    NewRow >=0,
    NewRow < BoardSize,
    nth0(Row, Board, PrevLine),
    replace_element(Column,empty,PrevLine,NewPrevLine),
    replace_element(Row, NewPrevLine, Board, MidFinalBoard),
    nth0(NewRow, MidFinalBoard, NextLine),
    replace_element(NewCol,Piece,NextLine,NewLine),
    replace_element(NewRow, NewLine, MidFinalBoard, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 6, Piece, FinalBoard, _,(NewRow,NewCol)):-
    find_piece_board(rock2, Board, 0, (NewRow,NewCol), BoardSize),
    NewCol >= 0,
    NewCol < BoardSize,
    NewRow >=0,
    NewRow < BoardSize,
    nth0(Row, Board, PrevLine),
    replace_element(Column,empty,PrevLine,NewPrevLine),
    replace_element(Row, NewPrevLine, Board, MidFinalBoard),
    nth0(NewRow, MidFinalBoard, NextLine),
    replace_element(NewCol,Piece,NextLine,NewLine),
    replace_element(NewRow, NewLine, MidFinalBoard, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 7, Piece, FinalBoard,_,(NewRow,NewCol)):-
    find_piece_board(rock3, Board, 0, (NewRow,NewCol), BoardSize),
    NewCol >= 0,
    NewCol < BoardSize,
    NewRow >=0,
    NewRow < BoardSize,
    nth0(Row, Board, PrevLine),
    replace_element(Column,empty,PrevLine,NewPrevLine),
    replace_element(Row, NewPrevLine, Board, MidFinalBoard),
    nth0(NewRow, MidFinalBoard, NextLine),
    replace_element(NewCol,Piece,NextLine,NewLine),
    replace_element(NewRow, NewLine, MidFinalBoard, FinalBoard),!.
change_board(Board, (Row, Column), BoardSize, 8, Piece, FinalBoard,_,(NewRow,NewCol)):-
    find_piece_board(rock4, Board, 0, (NewRow,NewCol), BoardSize),
    NewCol >= 0,
    NewCol < BoardSize,
    NewRow >=0,
    NewRow < BoardSize,
    nth0(Row, Board, PrevLine),
    replace_element(Column,empty,PrevLine,NewPrevLine),
    replace_element(Row, NewPrevLine, Board, MidFinalBoard),
    nth0(NewRow, MidFinalBoard, NextLine),
    replace_element(NewCol,Piece,NextLine,NewLine),
    replace_element(NewRow, NewLine, MidFinalBoard, FinalBoard),!.

% throw_rock(+Board, +BoardSize, +Position, +Direction, +Move, -FinalBoard)
% Throws Rock while possible in the given direction

throw_rock(Board, BoardSize, (Row, Column), 1, Move, FinalBoard):-
    NewRow is Row - 1,
    NewRow >= 0,
    NewRow < BoardSize,
    keep_throwing(Board,NewRow,Column,1,Move),
    throw_rock(Board, BoardSize, (NewRow, Column), 1, Move, FinalBoard).
throw_rock(Board,BoardSize,(Row, Column),2,Move,FinalBoard):-
    NewRow is Row +1,
    NewRow >= 0,
    NewRow  < BoardSize,
    keep_throwing(Board,NewRow,Column,2,Move),
    throw_rock(Board,BoardSize,(NewRow, Column),2,Move,FinalBoard).
throw_rock(Board,BoardSize,(Row, Column),3,Move,FinalBoard):-
    NewCol is Column -1,
    NewCol >= 0,
    NewCol  < BoardSize,
    keep_throwing(Board,Row,NewCol,3,Move),
    throw_rock(Board,BoardSize,(Row, NewCol),3,Move,FinalBoard).
throw_rock(Board,BoardSize,(Row, Column),4,Move,FinalBoard):-
    NewCol is Column + 1,
    NewCol >= 0,
    NewCol  < BoardSize,
    keep_throwing(Board,Row,NewCol,4,Move),
    throw_rock(Board,BoardSize,(Row, NewCol),4,Move,FinalBoard).
throw_rock(Board, _, (Row, Column), _, Move, FinalBoard):-
    nth0(Row, Board, Line),
    move_to_rock(Move,Rock),
    replace_element(Column, Rock, Line, NewLine),
    replace_element(Row, NewLine, Board, FinalBoard).

% keep_throwing(+Board, +Row, +Col, +Direction,+Move)
% Checks if the rock can be continuing being thrown in the given direction

keep_throwing(Board,Row,Col,_,_):-
    nth0(Row, Board, Line),
    nth0(Col, Line, Symbol),
    (is_empty(Symbol); is_sorcerer(Symbol)).
keep_throwing(Board, Row,Col,1,_):-
    NewRow is Row -1,
    nth0(Row, Board, Line),
    is_dwarf(Col,Line),
    nth0(NewRow, Board, NewLine),
    nth0(Col, NewLine, Symbol),
    (is_empty(Symbol);is_sorcerer(Symbol)).
keep_throwing(Board, Row,Col,2,_):-
    NewRow is Row + 1,
    nth0(Row, Board, Line),
    is_dwarf(Col,Line),
    nth0(NewRow, Board, NewLine),
    nth0(Col, NewLine, Symbol),
    (is_empty(Symbol);is_sorcerer(Symbol)).
keep_throwing(Board, Row,Col,3,_):-
    NewCol is Col -1,
    nth0(Row, Board, Line),
    is_dwarf(NewCol,Line),
    nth0(Row, Board, NewLine),
    nth0(NewCol, NewLine, Symbol),
    (is_empty(Symbol);is_sorcerer(Symbol)).
keep_throwing(Board, Row,Col,4,_):-
    NewCol is Col +1,
    nth0(Row, Board, Line),
    is_dwarf(NewCol,Line),
    nth0(Row, Board, NewLine),
    nth0(NewCol, NewLine, Symbol),
    (is_empty(Symbol);is_sorcerer(Symbol)).


% is_dwarf(+Col, +Line)
% Checks if in the given coordinates is placed a dwarf

is_dwarf(Col,Line):-
    nth0(Col, Line, dwarf1);nth0(Col, Line, dwarf2).

% game_over(+Board,+Row,+Column,+Direction,+Move,-State,+BoardSize, -FinalBoard)
% Gets final board and state after the play
    
game_over(Board,_,_,1,Move,State,BoardSize, FinalBoard):-
    move_to_rock(Move,Rock),
    find_piece_board(Rock, Board, 0, (NumberRow, FinalColumn), BoardSize),
    NewRow is NumberRow -1,
    NewRow>=0,
    change_state(NumberRow, FinalColumn, NewRow,FinalColumn,State, Rock, Board, FinalBoard, BoardSize).

game_over(Board,_,_,2,Move,State,BoardSize, FinalBoard):-
    move_to_rock(Move,Rock),
    find_piece_board(Rock, Board, 0, (NumberRow, FinalColumn), BoardSize),
    NewRow is NumberRow + 1,
    NewRow<BoardSize,
    change_state(NumberRow, FinalColumn, NewRow,FinalColumn,State, Rock, Board, FinalBoard, BoardSize).

game_over(Board,_,_,3,Move,State,BoardSize, FinalBoard):-
    move_to_rock(Move,Rock),
    find_piece_board(Rock, Board, 0, (NumberRow, FinalColumn), BoardSize),
    NewCol is FinalColumn -1,
    NewCol >=0,
    change_state(NumberRow, FinalColumn, NumberRow,NewCol,State, Rock, Board, FinalBoard, BoardSize).
game_over(Board,_,_,4,Move,State,BoardSize, FinalBoard):-
    move_to_rock(Move,Rock),
    find_piece_board(Rock, Board, 0, (NumberRow, FinalColumn), BoardSize),
    NewCol is FinalColumn +1,
    NewCol < BoardSize,
    change_state(NumberRow, FinalColumn, NumberRow,NewCol,State, Rock, Board, FinalBoard, BoardSize).
game_over(Board,_,_,_,_,0,_,Board).

% change_state(+RowRock, +ColRock, +RowSrc ,+ColSrc , -State, +Rock, +Board, -FinalBoard, +BoardSize)
% Replaces the rock and checks if any player won the game

change_state(RowRock, ColRock, RowSrc ,ColSrc ,State, Rock, Board, FinalBoard, _):-
    nth0(RowSrc, Board, Line),
    nth0(ColSrc, Line, sorcerer1), 
    nth0(RowRock, Board, LineRock),
    replace_element(ColRock, empty, LineRock, NewLineRock),
    replace_element(RowRock, NewLineRock, Board, MidFinalBoard),
    nth0(RowSrc, MidFinalBoard, NewLine),
    replace_element(ColSrc, Rock, NewLine, FinalLine),
    replace_element(RowSrc, FinalLine, MidFinalBoard, FinalBoard),
    State is 2.

change_state(_, _, _ , _ ,State, _, Board, Board, BoardSize):-
    find_piece_board(sorcerer1, Board, 0, (Row, _), BoardSize), 
    var(Row),
    State is 2.

change_state(RowRock, ColRock, RowSrc ,ColSrc ,State, Rock, Board, FinalBoard, _):-
    nth0(RowSrc, Board, Line),
    nth0(ColSrc, Line, sorcerer2),
    nth0(RowRock, Board, LineRock),
    replace_element(ColRock, empty, LineRock, NewLineRock),
    replace_element(RowRock, NewLineRock, Board, MidFinalBoard),
    nth0(RowSrc, MidFinalBoard, NewLine),
    replace_element(ColSrc, Rock, NewLine, FinalLine),
    replace_element(RowSrc, FinalLine, MidFinalBoard, FinalBoard),
    State is 1.

change_state(_, _, _ , _ ,State, _, Board, Board, BoardSize):-
    find_piece_board(sorcerer2, Board, 0, (Row, _), BoardSize), 
    var(Row),
    State is 1.

change_state(_, _, _ ,_ ,_, _, Board, Board, _).
