:- module(mod_jeu,[empty_board/1, setBoard/4, move/4, get_opponent/2]).
:- use_module('regles.pl').

% Initialisation Plateau vide
empty_board([0, 0, 0, 0, -1, 0, 0, 0, 0]).

% Modification du plateau
setBoard(Value, 0, [_|T], [Value|T])  :- !.
setBoard(Value, Index, [X|T], [X|Y]) :-
    Index > 0,
    Index1 is Index - 1,
    setBoard(Value, Index1, T, Y), !.

% Pose un pion
move(JR, PL, [-1, C, 0], NPL) :-
    !, deplacement(JR, PL, [-1, C, 0]),
    setBoard(JR, C, PL, NPL).

% Déplace un pion
move(JR, PL, [CA, CS, 1], NPL) :-
    !, deplacement(JR, PL, [CA, CS, 1]),
    setBoard(0, CA, PL, Temp),
    setBoard(JR, CS, Temp, NPL).

% Déplace une case
move(_, PL, [CA, CS, 2], NPL) :-
    !, deplacement(_, PL, [CA, CS, 2]),
    setBoard(-1, CS, PL, Temp),
    nth0(CS, PL, Val),
    setBoard(Val, CA, Temp, NPL).

% Déplace deux cases
move(_, PL, [C1, C2, 3], NPL) :-
    get_opposed(C1, C2),
    move(_, PL, [C1, CI, 2], Temp0),
    move(_, Temp0, [CI, C2, 2], Temp1),
    setBoard(-1, C2, Temp1, NPL).

get_opponent(1, 2) :- !.
get_opponent(2, 1).
