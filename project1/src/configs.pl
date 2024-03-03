:-consult(utils).
:-consult(board).

% get_game_level(-GameLevel)
% Receives input of GameLevel

get_game_level(GameLevel):-    
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    validate_input([1,2],GameLevel).

% game_mode(+GameMode, -GameLevel1, -GameLevel2)
% Defines levels for different players and modes

game_mode(1, GameLevel1, GameLevel2):-
    GameLevel1 is 0,
    GameLevel2 is 0.
game_mode(2, GameLevel1, GameLevel2):-
    write('Please select a game level\n'),
    get_game_level(GameLevel1),
    GameLevel2 is 0.
game_mode(3, GameLevel1, GameLevel2):-
    write('Please select a game level for computer 1\n'),
    get_game_level(GameLevel1),
    write('Please select a game level for computer 2\n'),
    get_game_level(GameLevel2).

% board_size(+Height, -Board)
% Creates initial board and displays it in function of the height

board_size(Height, Board):-
    Width is Height,
    initial_state(Height, Width, 0, Board),
    display_game(Board, 0, Width, 0).

% get_board_size(-Board, -BoardSize)
% Defines the size of the board and the board itself

get_board_size(Board, BoardSize):-
    write('Please select an odd value between 7 and 21 for the height of your board:\n'),
    validate_input([7,9,11,13,15,17,19,21],BoardSize),
    board_size(BoardSize, Board).

% get_first_player(+GameMode, -Player)
% Defines initial player

get_first_player(1, Player) :- 
    Player is 0.
get_first_player(2, Player) :-
    write('Please choose who plays first\n'),
    write('1 Human\n'),
    write('2 Computer\n'),
    validate_input([1,2],Choice),
    Player is Choice - 1.

get_first_player(3, Player) :-
    write('Please choose who plays first\n'),
    write('1 Computer 1\n'),
    write('2 Computer 2\n'),
    validate_input([1,2],Choice),
    Player is Choice - 1.