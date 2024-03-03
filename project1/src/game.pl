 :- consult(board).
 :- use_module(library(lists)).
 :- use_module(library(random)).

% game(+GameState, +GameMode, +GameLevel1, +GameLevel2, +NumberPlays)
% Keeps running the game by calling the right play of the rigth player depending on the round
game([ _, _, _, _, State], _, _, _, _):-
    State > 0.
game([Board, BoardSize, Player, 1, State],GameMode, GameLevel1, GameLevel2, 0):-
    NewPlayer is mod(Player + 1,2),
    game([Board, BoardSize, NewPlayer, 2, State],GameMode, GameLevel1, GameLevel2, 2).
game([Board, BoardSize, Player, 2, State],GameMode, GameLevel1, GameLevel2, 0):-
    NewPlayer is mod(Player + 1, 2),
    game([Board, BoardSize, NewPlayer, 3, State],GameMode, GameLevel1, GameLevel2, 3).
game([Board, BoardSize, Player, RoundNumber, State],GameMode, GameLevel1, GameLevel2, 0):-
    RoundNumber > 2,
    NewRoundNumber is RoundNumber + 1,
    NewPlayer is mod(Player + 1, 2),
    game([Board, BoardSize, NewPlayer, NewRoundNumber, State],GameMode, GameLevel1, GameLevel2, 3).
game([Board, BoardSize, Player, RoundNumber, State],GameMode, GameLevel1, GameLevel2, NumberPlays):-
    check_human(Player, GameMode),
    NumberPlays > 0,
    DisplayPlayer is Player + 1,
    write('PLAYER '), write(DisplayPlayer), nl,
    pick_piece([Board, BoardSize, Player, RoundNumber, State],Piece,BoardPiece,PositionPiece),
    pick_move(Board, PositionPiece, BoardSize, Move, Piece),
    pick_rock([Board, BoardSize, Player, RoundNumber, State],Piece,Rock,PositionPiece,Move,PositionRock), 
    move([Board, BoardSize, Player, RoundNumber, State],[FinalBoard, BoardSize, Player, RoundNumber, NewState],Piece,BoardPiece,PositionPiece,Rock,Move,PositionRock,1),
    display_game(FinalBoard, 0, BoardSize,NewState),
    NewNumberPlays is NumberPlays - 1,
    game([FinalBoard, BoardSize, Player, RoundNumber, NewState],GameMode, GameLevel1, GameLevel2, NewNumberPlays).
game([Board, BoardSize, Player, RoundNumber, State],GameMode, GameLevel1, GameLevel2, NumberPlays):-
    check_random_computer(Player, GameMode, GameLevel1, GameLevel2),
    NumberPlays > 0,
    DisplayPlayer is Player + 1,
    write('Press enter to continue:\n'),
    read_number(_),
    write('PLAYER '), write(DisplayPlayer), nl,
    choose_move(Board, BoardSize, Player, 1, Piece-Move-Rock),
    get_piece(Player, Piece, BoardPiece),
    find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize),
    get_piece(_, Rock, BoardRock),
    find_piece_board(BoardRock, Board, 0, PositionRock, BoardSize),
    write('Piece: '), write(BoardPiece), nl, 
    write('Move: '), print_move(Move), nl,
    write('Rock: '), write(BoardRock), nl,
    move([Board, BoardSize, Player, RoundNumber, State],[FinalBoard, BoardSize, Player, RoundNumber, NewState],Piece,BoardPiece,PositionPiece,BoardRock,Move,PositionRock,2),
    display_game(FinalBoard, 0, BoardSize,NewState),
    NewNumberPlays is NumberPlays - 1,
    game([FinalBoard, BoardSize, Player, RoundNumber, NewState],GameMode, GameLevel1, GameLevel2, NewNumberPlays).
game([Board, BoardSize, Player, RoundNumber, _],GameMode, GameLevel1, GameLevel2, NumberPlays):-
    NumberPlays > 0,
    DisplayPlayer is Player + 1,
    write('Press enter to continue:\n'),
    read_number(_),
    write('PLAYER '), write(DisplayPlayer), nl,
    choose_move(Board, BoardSize, Player, 2, Piece-Move-Rock-Direction),
    get_piece(Player, Piece, BoardPiece),
    find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize),
    get_piece(_, Rock, BoardRock),
    find_piece_board(BoardRock, Board, 0, PositionRock, BoardSize),
    write('Piece: '), write(BoardPiece), nl, 
    write('Move: '), print_move(Move), nl,
    write('Rock: '), write(BoardRock), nl,
    write('Direction: '), print_direction(Direction), nl, nl,
    move_rock(Board, Piece, PositionPiece, BoardRock, BoardSize, Move, PositionRock, NewPositionRock),
    change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(_,_)),
    change_rock_board(NewBoard, PositionRock, BoardSize, NewPositionRock, BoardRock, FinalBoard, Move, Direction, NewState),
    display_game(FinalBoard, 0, BoardSize,NewState),
    NewNumberPlays is NumberPlays - 1,
    game([FinalBoard, BoardSize, Player, RoundNumber, NewState],GameMode, GameLevel1, GameLevel2, NewNumberPlays).

% move(+GameState, -NewGameState,+Piece,+BoardPiece,+PositionPiece,+Rock,+Move,+PositionRock,+GameMode)
% Changes board after the move played

move([Board, BoardSize, Player, RoundNumber, _],[FinalBoard, BoardSize, Player, RoundNumber, NewState],Piece,BoardPiece,PositionPiece,Rock,Move,PositionRock,1):-
    move_rock(Board, Piece, PositionPiece, Rock, BoardSize, Move, PositionRock, NewPositionRock),
    change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(NewRow,NewCol)),
    pick_rock_throw(NewBoard, BoardSize,(NewRow,NewCol),Move, Direction),
    change_rock_board(NewBoard, PositionRock, BoardSize, NewPositionRock, Rock, FinalBoard,Move, Direction,NewState),!.
move([Board, BoardSize, Player, RoundNumber, _],[FinalBoard, BoardSize, Player, RoundNumber, NewState],Piece,BoardPiece,PositionPiece,Rock,Move,PositionRock,2):-
    move_rock(Board, Piece, PositionPiece, Rock, BoardSize, Move, PositionRock, NewPositionRock),
    change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(NewRow,NewCol)),
    choose_rock_throw(NewBoard,BoardSize,(NewRow,NewCol), Direction, Move),
    change_rock_board(NewBoard, PositionRock, BoardSize, NewPositionRock, Rock, FinalBoard,Move, Direction,NewState),!.

% move(+GameState, -NewGameState, +Play)
% Changes board after according to the chosen play
move([Board, BoardSize, Player, RoundNumber, _],[FinalBoard, BoardSize, Player, RoundNumber, NewState],Piece-Move-Rock-Direction):-
    get_piece(Player,Piece,BoardPiece),
    find_piece_board(BoardPiece,Board,0,PositionPiece,BoardSize),
    get_piece(_,Rock,BoardRock),
    find_piece_board(BoardRock,Board,0,PositionRock,BoardSize),
    move_rock(Board, Piece, PositionPiece, BoardRock, BoardSize, Move, PositionRock, NewPositionRock),
    change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(_,_)),
    change_rock_board(NewBoard, PositionRock, BoardSize, NewPositionRock, BoardRock, FinalBoard,Move, Direction,NewState),!.



% check_human(+Player, +GameMode)
% Checks if current player is human
check_human(1, 1).
check_human(0, GameMode):-
    GameMode =:= 1;
    GameMode =:= 2.

% check_random_computer(+Player, +GameMode, +GameLevel1, +GameLevel2)
% checks if current player is random bot
check_random_computer(Player, 2, GameLevel1, _):-
    Player =:= 1, GameLevel1 =:= 1.

check_random_computer(Player, 3, GameLevel1, GameLevel2):-
    Player =:= 0, GameLevel1 =:= 1;
    Player =:= 1, GameLevel2 =:= 1.

% value(+GameState, -FinalValue)
% Assigns a value to a play according to the state of the game after that same play

value([Board, BoardSize, Player, _, _],FinalValue):-
    OponentPlayer is mod(Player+1,2),
    get_piece(OponentPlayer,1,Sorcerer),
    find_piece_board(Sorcerer,Board,0,(Row,_),BoardSize),
    var(Row),
    FinalValue is 100000,!.
value([Board, BoardSize, Player, _, _],FinalValue):-
    get_piece(Player,1,Sorcerer),
    find_piece_board(Sorcerer,Board,0,(Row,_),BoardSize),
    var(Row),
    FinalValue is 0 - 100000,!.
value([Board, BoardSize, Player, _, _],FinalValue):-
    Value is 0,
    get_piece(Player,3,Troll),
    OponentPlayer is mod(Player+1,2),
    get_piece(OponentPlayer,1,Sorcerer),
    get_piece(Player,1,OurSorcerer),
    find_piece_board(Sorcerer,Board,0,SorcererPosition,BoardSize),
    find_piece_board(OurSorcerer,Board,0,OurSorcererPosition,BoardSize),
    find_piece_board(Troll,Board,0,TrollPosition,BoardSize),
    find_piece_board(rock1,Board,0,Rock1Position,BoardSize),
    find_piece_board(rock2,Board,0,Rock2Position,BoardSize),
    find_piece_board(rock3,Board,0,Rock3Position,BoardSize),
    find_piece_board(rock4,Board,0,Rock4Position,BoardSize),
    rock_aligned(Rock1Position,SorcererPosition,TrollPosition,Value,NewValue),
    rock_aligned(Rock2Position,SorcererPosition,TrollPosition,NewValue,NewValue2),
    rock_aligned(Rock3Position,SorcererPosition,TrollPosition,NewValue2,NewValue3),
    rock_aligned(Rock4Position,SorcererPosition,TrollPosition,NewValue3,NewValue4),
    not_aligned(Rock1Position,OurSorcererPosition,NewValue4,NewValue5),
    not_aligned(Rock2Position,OurSorcererPosition,NewValue5,NewValue6),
    not_aligned(Rock3Position,OurSorcererPosition,NewValue6,NewValue7),
    not_aligned(Rock4Position,OurSorcererPosition,NewValue7,FinalValue),!.

% not_aligned(+RockCordinates, +SorcererCordinates, +Value, -FinalValue)
% Adds value to the play if the sorcerer of the player is not aligned with a rock

not_aligned((RockRow,RockCol),(SrcRow,SrcCol),Value,FinalValue):-
   \+ ( RockRow =:= SrcRow;RockCol =:= SrcCol),
    FinalValue is Value + 10.
not_aligned(_,_,Value,Value).

% rock_aligned(+RockCordinates, +SorcererCordinates, +TrollCordinates, +Value, -FinalValue)
% Adds value to the play if the opposite player sorcerer is aligned with a rock
rock_aligned((RockRow,RockCol),(SrcRow,SrcCol),(TrollRow,TrollCol),Value,FinalValue):-
   ( RockRow =:= SrcRow;RockCol =:= SrcCol),
   NewValue is Value + 40,
   troll_adjacent((RockRow,RockCol),(TrollRow,TrollCol),NewValue,FinalValue).
rock_aligned(_,_,_,Value,Value).

% troll_adjacent(+RockCordinates, +TrollCordinates, +Value, -FinalValue)
% Adds value to a play in proportion of how close the player troll is of one the rocks aligned to the oposite player sorcerer

troll_adjacent((RockRow,RockCol),(TrollRow,TrollCol),Value,FinalValue):-
    DifRow is RockRow-TrollRow,
    DifCol is RockCol - TrollCol,
    Dist is (DifRow*DifRow) + (DifCol*DifCol),
    Dist2 is round((1/Dist)*100),
    FinalValue is Value+(Dist2).

troll_adjacent(_,_,Value,Value).

% move_rock(+Board, +Piece, +PositionPiece, +Rock, +BoardSize, +Move, ?PositionRock, -NewPositionRock)
% Finds position of the rock in the board after the chosen move

move_rock(_, _, _, 0, _, Move, _, _):-
    Move < 5,!.
move_rock(_,1,_,_,_,1,(NumberRow,NumberColumn),(NewNumberRow,NewNumberColumn)):-
    NumberRow > 0,
    NewNumberRow is  NumberRow -1,
    NewNumberColumn is NumberColumn,!.
move_rock(_,1,_,_,_,2,(NumberRow,NumberColumn),(NewNumberRow,NewNumberColumn)):-
    NewNumberRow is  NumberRow +1,
    NewNumberColumn is NumberColumn,!.
move_rock(_,1,_,_,_,3,(NumberRow,NumberColumn),(NewNumberRow,NewNumberColumn)):-
    NumberColumn > 0,
    NewNumberRow is NumberRow,
    NewNumberColumn is NumberColumn - 1,!.
move_rock(_,1,_,_,_,4,(NumberRow,NumberColumn),(NewNumberRow,NewNumberColumn)):-
    NewNumberRow is  NumberRow,
    NewNumberColumn is NumberColumn + 1,!.
move_rock(_,1,_,_,_,_,(NumberRow,NumberColumn),(NumberRow,NumberColumn)):-!.
move_rock(Board, 3, _, _, BoardSize, Move, PositionRock, _):-
    Move > 4,
    move_to_rock(Board,BoardSize,Move,PositionRock),!.
move_rock(_, 3, PositionPiece, _, _, _, _, PositionPiece):-!.

% move_to_rock(+Board, +BoardSize, +Move, -Position)
% Gets position of the specific rock according to the move

move_to_rock(Board, BoardSize,5,Position):-
    find_piece_board(rock1, Board, 0, Position, BoardSize).
move_to_rock(Board, BoardSize,6,Position):-
    find_piece_board(rock2, Board, 0, Position, BoardSize).
move_to_rock(Board, BoardSize,7,Position):-
    find_piece_board(rock3, Board, 0, Position, BoardSize).
move_to_rock(Board, BoardSize,8,Position):-
    find_piece_board(rock4, Board, 0, Position, BoardSize).

% possible_rocks(+Board, +Piece, +PositionPiece, +StartRock, -FinalListofRocks, +Move)
% Gets all the possible rocks a player can choose to play according to the piece chosen

possible_rocks(_, _, _, 0, [],_).
possible_rocks(_, 1, _, 1, [Rock|Rest],_):-
    Rock is 4,
    possible_rocks(_, 1, _, 0, Rest,_), !.
possible_rocks(_, 1, _, StartRock, [Rock|Rest],_):-
    Rock is StartRock + 3,
    NewStartRock is StartRock - 1,
    possible_rocks(_, 1, _, NewStartRock, Rest,_), !.
possible_rocks(_, 2, (_, _), _, FinalListofRocks,_):-
    append([4], [], FinalListofRocks), !.
possible_rocks(Board, 3, (Row, Column), _, FinalListofRocks,Move):-
    UpRow is Row - 1,
    DownRow is Row + 1,
    LeftColumn is Column - 1,
    RightColumn is Column + 1,
    check_rock(Board, UpRow, Column, [4], ListofRocks1,Move,2),
    check_rock(Board, DownRow, Column, ListofRocks1, ListofRocks2,Move,1),
    check_rock(Board, Row, LeftColumn, ListofRocks2, ListofRocks3,Move,4),
    check_rock(Board, Row, RightColumn, ListofRocks3, FinalListofRocks,Move,3), !.

% print_rocks(+Rocks, -RockOptions)
% Prints all the possible rocks to choose

print_rocks([], []).
print_rocks([4|Rest],[1|RestOption]):-
    write('1 - none\n'),
    print_rocks(Rest,RestOption).
print_rocks([Rock|Rest],[RockOption|RestOption]):-
    RockOption is Rock - 3,
    write(RockOption), write(' - '), 
    get_piece(_, Rock, RockBoard),
    write(RockBoard),nl,
    print_rocks(Rest,RestOption).

% pick_rock(+GameState, +Piece, -BoardRock, +PiecePosition, +Move, -PositionRock)
% Gets input of the chosen rock

pick_rock([Board, BoardSize, _, _, _],Piece,BoardRock,(Row, Column),Move,PositionRock):-
    possible_rocks(Board, Piece, (Row, Column), 5, ListofRocks,Move),
    write('Choose the rock you want to pick:\n'),
    sort(ListofRocks, SortedListofRocks),
    print_rocks(SortedListofRocks,OptionList),
    validate_input(OptionList,Rock),
    FinalRock is Rock + 3,
    get_piece(_, FinalRock, BoardRock),
    find_piece_board(BoardRock, Board, 0, PositionRock, BoardSize).

% check_rock(+Board, +Row, +Column, +ListofRocks, -NewListofRocks, +Move, +ExpectedMove)
% Adds values of rocks to the list of possible rocks

check_rock(Board, Row, Column, ListofRocks, NewListofRocks,Move,ExpectedMove):-
    nth0(Row, Board, Line),
    nth0(Column, Line, Piece),
    get_piece(_, Option, Piece),
    Option >= 5,
    Option =< 8,
    Move =:= ExpectedMove,
    append(ListofRocks, [Option], NewListofRocks).
check_rock(_, _, _, ListofRocks, ListofRocks,_,_).


% pick_piece(+GameState, -Piece, -BoardPiece, -PositionPiece)
% Gets input of the chosen piece

pick_piece([Board, BoardSize, Player, _, _],Piece,BoardPiece,PositionPiece):-
    write('Choose the piece you want to play:\n'),
    write('1 Sorcerer\n'),
    write('2 Dwarf\n'),
    write('3 Troll\n'),
    validate_input([1,2,3],Piece),
    get_piece(Player, Piece, BoardPiece),
    find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize).

% print_moves(+ListofMoves)
% Prints all possible moves

print_moves([]).
print_moves([Move|Rest]):-
    write(Move), write( ' - '),
    print_move(Move),nl,
    print_moves(Rest).

% possible_moves(+Board, +PositionPiece, +BoardSize, +Piece, -ListofMoves)
% Checks all possible moves for a chosen piece

possible_moves(Board, (Row, Column), BoardSize, 3,FinalListofMoves):-
    UpRow is Row - 1,
    DownRow is Row + 1,
    LeftColumn is Column - 1,
    RightColumn is Column + 1,
    check_empty_troll(Board, UpRow, Column, BoardSize, 'up', 1,[],ListofMoves1),
    check_empty_troll(Board, DownRow, Column, BoardSize, 'down', 2,ListofMoves1,ListofMoves2),
    check_empty_troll(Board, Row, LeftColumn, BoardSize, 'left', 3,ListofMoves2,ListofMoves3),
    check_empty_troll(Board, Row, RightColumn, BoardSize, 'right', 4,ListofMoves3,FinalListofMoves), !.
possible_moves(Board, (Row, Column), BoardSize, 2,FinalListofMoves):-
    UpRow is Row - 1,
    DownRow is Row + 1,
    LeftColumn is Column - 1,
    RightColumn is Column + 1,
    check_empty_dwarf(Board, UpRow, Column, BoardSize, 'up', 1,[],ListofMoves1),
    check_empty_dwarf(Board, DownRow, Column, BoardSize, 'down', 2,ListofMoves1,ListofMoves2),
    check_empty_dwarf(Board, Row, LeftColumn, BoardSize, 'left', 3,ListofMoves2,ListofMoves3),
    check_empty_dwarf(Board, Row, RightColumn, BoardSize, 'right', 4,ListofMoves3,FinalListofMoves), !.   
possible_moves(Board, (Row, Column), BoardSize, _,FinalListofMoves):-
    UpRow is Row - 1,
    DownRow is Row + 1,
    LeftColumn is Column - 1,
    RightColumn is Column + 1,
    check_empty(Board, UpRow, Column, BoardSize, 1,[],ListofMoves1),
    check_empty(Board, DownRow, Column, BoardSize, 2,ListofMoves1,ListofMoves2),
    check_empty(Board, Row, LeftColumn, BoardSize, 3,ListofMoves2,ListofMoves3),
    check_empty(Board, Row, RightColumn, BoardSize, 4,ListofMoves3,FinalListofMoves), !.
possible_moves(_,_,_,_,_).

% pick_move(+Board, +PositionPiece, +BoardSize, -Move, +Piece)
% Gets input of the chosen move

pick_move(Board, (Row, Column), BoardSize, Move, Piece):-
    possible_moves(Board, (Row, Column), BoardSize, Piece,ListofMoves),
    write('Choose a move:\n'),
    sort(ListofMoves, SortedListofMoves),
    print_moves(SortedListofMoves),
    validate_input(SortedListofMoves,Move).


% print_directions(ListofDirections)
% Prints all possible direction to throw a rock

print_directions([]).
print_directions([Direction|Rest]):-
    write(Direction), write( ' - '),
    print_direction(Direction),nl,
    print_directions(Rest).

% possible_directions(+Board, +PositionPiece, +BoardSize, -ListofDirections, +Move)
% Checks all possible directions to throw rock

possible_directions(_, (_, _), _,[0],Move):-
    Move < 5,
    ! .
possible_directions(Board, (Row, Column), BoardSize,FinalListofMoves,Move):-
    Move > 4,
    check_rock_throw(Board,BoardSize,(Row, Column),1,[],ListofMoves1),
    check_rock_throw(Board,BoardSize,(Row, Column),2,ListofMoves1,ListofMoves2),
    check_rock_throw(Board,BoardSize,(Row, Column),3,ListofMoves2,ListofMoves3),
    check_rock_throw(Board,BoardSize,(Row, Column),4,ListofMoves3,FinalListofMoves), !.
possible_directions(_,_,_,_,_):-!.

% pick_rock_throw(+Board, +BoardSize, +PiecePosition, +Move, -Direction)
% Gets input of chosen direction

pick_rock_throw(Board,BoardSize,(NewRow,NewCol),Move, FinalDirection):-
    Move > 4,
    possible_directions(Board,(NewRow,NewCol),BoardSize,ListofMoves,Move),
    write('Choose direction to throw rock:\n'), 
    sort(ListofMoves,SortedListofMoves),
    print_directions(SortedListofMoves),
    validate_input(SortedListofMoves,FinalDirection).
pick_rock_throw(_,_,(_,_),_, 0).


% check_rock_throw(+Board,+BoardSize,+PiecePosition,+Direction,+ListofMoves,-FinalListofMoves)
% Adds option of direction if it is possible to throw the rock

check_rock_throw(Board,BoardSize,(Row,Column),1,ListofMoves,FinalListofMoves):-
    NewRow is Row - 1,
    NewRow >= 0,
    NewRow < BoardSize,
    keep_throwing(Board, NewRow,Column,1,_),
    append(ListofMoves, [1], FinalListofMoves).
check_rock_throw(Board,BoardSize,(Row,Column),2,ListofMoves,FinalListofMoves):-
    NewRow is Row + 1,
    NewRow >= 0,
    NewRow < BoardSize,
    keep_throwing(Board, NewRow,Column,2,_),
    append(ListofMoves, [2], FinalListofMoves).
check_rock_throw(Board,BoardSize,(Row,Column),3,ListofMoves,FinalListofMoves):-
    NewCol is Column - 1,
    NewCol >= 0,
    NewCol < BoardSize,
    keep_throwing(Board, Row,NewCol,3,_),
    append(ListofMoves, [3], FinalListofMoves).
check_rock_throw(Board,BoardSize,(Row,Column),4,ListofMoves,FinalListofMoves):-
    NewCol is Column + 1,
    NewCol >= 0,
    NewCol < BoardSize,
    keep_throwing(Board, Row,NewCol,4,_),
    append(ListofMoves, [4], FinalListofMoves).
check_rock_throw(_, _, _, _, ListofMoves, ListofMoves).

% check_empty(+Board, +Row, +Column, +BoardSize, +Choice, +ListofMoves, -NewListofMoves)
% Adds option to possible moves if cell is empty

check_empty(Board, Row, Column, BoardSize, Choice,ListofMoves,NewListofMoves):-
    Row >= 0,
    Row < BoardSize,
    Column >= 0,
    Column < BoardSize,
    nth0(Row, Board, Line),
    nth0(Column, Line, empty),
    append(ListofMoves, [Choice], NewListofMoves).
check_empty(_, _, _, _, _,ListofMoves,ListofMoves).

% check_empty_dwarf(+Board, +Row, +Column, +BoardSize, +Symbol, +Choice, +ListofMoves, -NewListofMoves)
% Adds option of moving if cell is empty or has a rock and empty in the next cell

check_empty_dwarf(Board, Row, Column, BoardSize, Symbol, Choice,ListofMoves,NewListofMoves):-
    Row >= 0,
    Row < BoardSize,
    Column >= 0,
    Column < BoardSize,
    nth0(Row, Board, Line),
    (nth0(Column, Line, empty);check_next_cell(Board, Row, Column, Line, BoardSize, Symbol, Choice)),
    append(ListofMoves, [Choice], NewListofMoves).
check_empty_dwarf(_, _, _, _, _, _,ListofMoves,ListofMoves).

% check_next_cell(+Board, +Row, +Column, +Line, +BoardSize, +Symbol, +Choice)
% Checks if the next cell, after a rock, in the same direction is empty

check_next_cell(Board, Row, Column, Line, _, _, 1):-
    (nth0(Column, Line, rock1);nth0(Column, Line, rock2);nth0(Column, Line, rock3);nth0(Column, Line, rock4)),
    NewRow is Row - 1,
    NewRow >= 0,
    nth0(NewRow, Board, NewLine),
    nth0(Column, NewLine, empty).
check_next_cell(Board, Row, Column, Line, BoardSize, _, 2):-
    (nth0(Column, Line, rock1);nth0(Column, Line, rock2);nth0(Column, Line, rock3);nth0(Column, Line, rock4)),
    NewRow is Row + 1,
    NewRow <BoardSize,
    nth0(NewRow, Board, NewLine),
    nth0(Column, NewLine, empty).
check_next_cell(Board, Row, Column, Line, _, _, 3):-
    (nth0(Column, Line, rock1);nth0(Column, Line, rock2);nth0(Column, Line, rock3);nth0(Column, Line, rock4)),
    NewCol is Column - 1,
    NewCol >= 0,
    nth0(Row, Board, NewLine),
    nth0(NewCol, NewLine, empty).
check_next_cell(Board, Row, Column, Line, BoardSize, _, 4):-
    (nth0(Column, Line, rock1);nth0(Column, Line, rock2);nth0(Column, Line, rock3);nth0(Column, Line, rock4)),
    NewCol is Column + 1,
    NewCol < BoardSize,
    nth0(Row, Board, NewLine),
    nth0(NewCol, NewLine, empty).

% check_empty_troll(+Board, +Row, +Column, +BoardSize, +Symbol, +Choice, +ListofMoves, -NewListofMoves)
% Adds option if the next cell in the chosen direction is empty or has rock

check_empty_troll(Board, Row, Column, BoardSize, Symbol, Choice,ListofMoves,NewListofMoves):-
    Row >= 0,
    Row < BoardSize,
    Column >= 0,
    Column < BoardSize,
    nth0(Row, Board, Line),
    nth0(Column, Line, CurrSymbol),
    use_symbol_troll(CurrSymbol,Symbol,Choice,ListofMoves,NewListofMoves).
check_empty_troll(_, _, _, _, _,ListofMoves,ListofMoves).

% use_symbol_troll(+CurrSymbol, +Symbol, +Choice, +ListofMoves, -NewListofMoves)
% Verifies if cell is empty or with rock and adds the corresponding option for move in the final list

use_symbol_troll(empty,_,Choice,ListofMoves,NewListofMoves):-
    append(ListofMoves, [Choice], NewListofMoves).
use_symbol_troll(rock1,_,_,ListofMoves,NewListofMoves):-
    append(ListofMoves, [5], NewListofMoves).
use_symbol_troll(rock2,_,_,ListofMoves,NewListofMoves):-
    append(ListofMoves, [6], NewListofMoves).
use_symbol_troll(rock3,_,_,ListofMoves,NewListofMoves):-
    append(ListofMoves, [7], NewListofMoves).
use_symbol_troll(rock4,_,_,ListofMoves,NewListofMoves):-
    append(ListofMoves, [8], NewListofMoves).
use_symbol_troll(_,_,_,ListofMoves,ListofMoves).


% choose_move(+Board, +BoardSize, +Player, +Bot, -Move)
% Chooses a play out of all the possibilities
% Completely random if bot 1
% Random between the ones with bigger value if bot 2

choose_move(Board, BoardSize, Player, 1, Piece-Move-Rock):-
    valid_moves(Board, BoardSize, Player, ListofMoves, 1),
    random_member(Piece-Move-Rock, ListofMoves).
choose_move(Board, BoardSize, Player, 2, Piece-Move-Rock-Direction):-
    valid_moves(Board, BoardSize, Player, ListofMoves, 2),
    findall(Value-Piece-Move-Rock-Direction, ( member(Piece-Move-Rock-Direction, ListofMoves), 
                            move([Board, BoardSize, Player, _, 0],[NewBoard, _, _, _, _],Piece-Move-Rock-Direction), 
                            value([NewBoard, BoardSize, Player, _, _],Value)),
                             Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_-_-_-_),
    findall(Piece-Move-Rock-Direction, member(Max-Piece-Move-Rock-Direction, SortedPairs), MaxMoves),
    random_member(Piece-Move-Rock-Direction, MaxMoves).
    
% valid_moves(+Board, +BoardSize, +Player, -ListofMoves, +Bot)
% Finds all valid moves for that player in function of the current board                      

valid_moves(Board, BoardSize, Player, ListofMoves, 1):-
    findall(Piece-Move-Rock, validate_moves(Piece, Move, Rock, Player, Board, BoardSize), ListofMoves).


valid_moves(Board, BoardSize, Player, ListofFinalMoves, 2):-
    findall(Piece-Move-Rock, validate_moves(Piece, Move, Rock, Player, Board, BoardSize), ListofMoves),
    findall(Piece-Move-Rock-Direction,(
                                        member(Piece-Move-Rock,ListofMoves),
                                       get_piece(Player, Piece, BoardPiece),
                                       find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize),
                                       get_piece(_, Rock, BoardRock),
                                       find_piece_board(BoardRock, Board, 0, PositionRock, BoardSize),
                                       move_rock(Board, Piece, PositionPiece, BoardRock, BoardSize, Move, PositionRock, _),
                                       change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(NewRow,NewCol)),
                                       possible_directions(NewBoard, (NewRow, NewCol), BoardSize, ListofDirections,Move),
                                       member(Direction,ListofDirections)),
                                       ListofFinalMoves).

% validate_moves(-Piece, -Move, -Rock, +Player, +Board, +BoardSize)
% Check if move is possible according to the current play and board

validate_moves(Piece, Move, Rock, Player, Board, BoardSize):-
    member(Piece, [1,2,3]),

    get_piece(Player, Piece, BoardPiece),
    find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize),

    possible_moves(Board, PositionPiece, BoardSize, Piece, ListofMoves),
    member(Move, ListofMoves),

    possible_rocks(Board, Piece, PositionPiece, 5, ListofRocks,Move),
    member(Rock, ListofRocks).

% choose_rock_throw(+Board, +BoardSize, +Position, -FinalDirection, +Move)
% Chooses a random direction to throw rock if possible

choose_rock_throw(_, _, _, 0, Move):-
    Move < 5, nl.
choose_rock_throw(Board,BoardSize,(NewRow,NewCol), FinalDirection, Move):-
    findall(Direction, validate_direction(Direction, Board, (NewRow, NewCol), BoardSize ,Move), ListofDirections),
    random_member(FinalDirection, ListofDirections),
    write('Direction: '), print_direction(FinalDirection), nl, nl.

% validate_direction(-Direction, +Board, +Position, +BoardSize, +Move)
% Check if direction is possible

validate_direction(Direction, Board, (Row, Column), BoardSize,Move):-
    possible_directions(Board, (Row, Column), BoardSize, ListofDirections,Move),
    member(Direction, ListofDirections).



