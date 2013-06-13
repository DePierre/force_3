:- use_module('gui.pl').

% Welcome prompt
welcome :-
    nl,
    writeln('Projet Force 3'),
    writeln('Créé dans le cadre du cours IA41 (UTBM)'), nl,
    writeln('-----Auteurs------------------'),
    writeln('|\tJulien Voisin        |'),
    writeln('|\tTao Sauvage          |'),
    writeln('|\tRobin Faury          |'),
    writeln('------------------------------'),
    nl.

% Menu pour la difficulté des IA
level_ia :-
    menu_ia(1, Level1),
    menu_ia(2, Level2),
    game_ia(Level1, Level2).

menu_ia(ID, Level) :-
    write('-----Niveau IA '), write(ID), writeln('--------------'),
    writeln('|  0.\tFacile               |'),
    writeln('|  1.\tMoyen                |'),
    writeln('|  2.\tDifficile            |'),
    writeln('------------------------------'),
    ask_id(Level).

% Affiche le menu
main :-
    welcome,
    init_ui,
    writeln('-----Menu---------------------'),
    writeln('|  0.\tJouer (J vs. IA)     |'),
    writeln('|  1.\tObserver (IA VS. IA) |'),
    writeln('|............................|'),
    writeln('|  2.\tQuitter              |'),
    writeln('------------------------------'),
    ask_id(Choix),
    start(Choix).

% Lance en fonction du choix du joueur
start(0) :-
    play.
start(1) :-
    level_ia.
start(_) :- !.

% Lance le menu
:- main.
