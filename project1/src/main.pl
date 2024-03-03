:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(configs).
:- consult(utils).
:- consult(game).

% starting_menu(-GameState, -GameMode, -GameLevel1, -GameLevel2)
% Prints starting menu, receives user input with configurations and creates the initial board
starting_menu([Board,BoardSize,Player,1,0],GameMode, GameLevel1, GameLevel2):-
    write('--------------------------------------------------------------------------\n'),
    write('\n'),
    write('                                 SPLUT!                                \n'),
    write('\n'),
    write('--------------------------------------------------------------------------\n'),
    write('\n'),
    write('Please choose which mode you prefer\n'),
    write('1 Human vs. Human\n'),
    write('2 Human vs. Computer\n'),
    write('3 Computer vs. Computer\n'),
    validate_input([1,2,3],GameMode),
    game_mode(GameMode, GameLevel1, GameLevel2),
    get_first_player(GameMode, Player),
    get_board_size(Board, BoardSize).

% play/0
% Starts the game
play:-
    starting_menu(GameState,GameMode, GameLevel1, GameLevel2),
    game(GameState, GameMode, GameLevel1, GameLevel2, 1).
