% read_number(-Number)
% Unifies Number with input

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

% validate_input(+List, +Input)
% Checks if input is part of the accepted ones

validate_input(List,Value):-
    read_number(Value),
    member(Value,List).
validate_input(List,Value):-
    repeat,
    write('Please insert a valid number:\n'),
    read_number(Value),
    member(Value,List),!.

% replace_element(+Index, +NewElement, +List, -NewList)
% Replace element in List

replace_element(Index, NewElement, List, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElement, Rest).

% print_move(+Move)
% Print correspondant move

print_move(1):- write('up').
print_move(2):- write('down').
print_move(3):- write('left').
print_move(4):- write('right').
print_move(5):- write('throw rock1').
print_move(6):- write('throw rock2').
print_move(7):- write('throw rock3').
print_move(8):- write('throw rock4').

% print_move(+Direction)
% Print correspondant direction

print_direction(0):- write('does not throw rock').
print_direction(1):- write('throw rock up').
print_direction(2):- write('throw rock down').
print_direction(3):- write('throw rock left').
print_direction(4):- write('throw rock right').

% is_rock(+Symbol)
% Checks if Symbol is a rock

is_rock(rock1).
is_rock(rock2).
is_rock(rock3).
is_rock(rock4).

% is_dwarf(+Symbol)
% Checks if Symbol is a dwarf

is_dwarf(dwarf1).
is_dwarf(dwarf2).

% is_sorcerer(+Symbol)
% Checks if Symbol is a sorcerer

is_sorcerer(sorcerer1).
is_sorcerer(sorcerer2).

% is_empty(+Symbol)
% Checks if Symbol is empty

is_empty(empty).

% move_to_rock(+Move, -Symbol)
% Gets rock correspondant to the move

move_to_rock(5,rock1).
move_to_rock(6,rock2).
move_to_rock(7,rock3).
move_to_rock(8,rock4).