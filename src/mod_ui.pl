:- module(mod_ui, [init_ui/0, game_ia/0, play_ia/0, play/0, welcome/0]).
:- use_module('mod_jeu.pl').
:- use_module('mod_regles.pl').
:- use_module('mod_eval.pl').

% Stock l'état du plateau de jeu
:- dynamic board/1.

% Initialise l'interface utilisateur
init_ui:-
    retractall(board(_)),
    empty_board(P),
    assert(board(P)).

% Demande un coup à l'utilisateur
ask_placement(PL, [C1,C2,ID]) :-
    display_coords(PL), nl,
    writeln('Choix disponibles :'),
    writeln('\t0.\tPoser un pion'),
    writeln('\t1.\tDéplacer un pion'),
    writeln('\t2.\tDéplacer le taquin'),
    ask_id(ID),
    ID == 0 -> ID is 0, C1 is -1, ask_dest(C2);
    ID == 2 -> ask_orig(C1), ask_dest(C2), id_move(C2,C1,ID);
    ask_orig(C1), ask_dest(C2).

ask_id(ID) :-
    read(ID), integer(ID), between(0, 3, ID),!.
ask_id(ID) :-
    writeln('Choix invalide. Reprécisez.'),
    ask_id(ID).

ask_orig(C) :-
    write('Coordonnée (origine) : '), read(C), integer(C), between(0, 8, C),!.
ask_orig(C) :-
    writeln('Coordonnée invalide. Reprécisez.'),
    ask_orig(C).

ask_dest(C) :-
    write('Coordonnée (destination) : '), read(C), integer(C), between(0, 8, C), !.
ask_dest(C) :-
    writeln('moveement invalide. Reprécisez.'),
    ask_dest(C).

% Récupère l'id du moveement effectué
id_move(0, C2, 2) :- member(C2, [1,3]).
id_move(1, C2, 2) :- member(C2, [0,2,4]).
id_move(2, C2, 2) :- member(C2, [1,5]).
id_move(3, C2, 2) :- member(C2, [0,4,6]).
id_move(4, C2, 2) :- member(C2, [1,5,7,3]).
id_move(5, C2, 2) :- member(C2, [2,8,4]).
id_move(6, C2, 2) :- member(C2, [3,7]).
id_move(7, C2, 2) :- member(C2, [4,8,6]).
id_move(8, C2, 2) :- member(C2, [5,7]).

id_move(0, C2, 3) :- member(C2, [6,2]).
id_move(1, 7, 3).
id_move(2, C2, 3) :- member(C2, [0,8]).
id_move(3, 5, 3).
id_move(5, 3, 3).
id_move(6, C2, 3) :- member(C2, [0,8]).
id_move(7, 1, 3).
id_move(8, C2, 3) :- member(C2, [2,6]).


% Sauvegarde le coup joué
save_play(Joueur,Coup) :-
    retract(board(B)),
    move(Joueur,B,Coup,B1),
    assert(board(B1)).

% IA vs. IA
game_ia :-
    board(PL),
    alpha_beta(1,6,PL,-200,200,Coup,_Valeur),!,
    save_play(1,Coup),
    board(NPL),
    display_board(NPL),
    not(won),
    alpha_beta(2,5,NPL,-200,200,Coup2,_Valeur2),!,
    save_play(2,Coup2),
    board(NPL2),
    display_board(NPL2),
    not(won),
    game_ia.

% Fait jouer l'IA
play_ia :-
    board(PL),
    alpha_beta(1,4,PL,-200,200,Coup,_Valeur),!,
    save_play(1,Coup),
    board(NPL),
    display_board(NPL),
    not(won),
    play.

% Demande au joueur de jouer
play :-
    board(PL),
    ask_placement(PL,Coup),
    save_play(2,Coup),
    board(NPL),
    display_board(NPL),
    not(won),
    play_ia.

% Vérifie si le joueur a gagné et propose de recommencer.
won :-
    board(PL),
    win(JR, PL),
    writef("Le joueur %w a gagné !", [JR]),
    init_ui,!.

% Affiche le plateau de jeu
display_board([C1,C2,C3,C4,C5,C6,C7,C8,C9]):-
    write('    _______'), nl,
    write('    |'),afc(C1),write(' '),afc(C2),write(' '),afc(C3),write('|'), nl,
    write('    |'),afc(C4),write(' '),afc(C5),write(' '),afc(C6),write('|'), nl,
    write('    |'),afc(C7),write(' '),afc(C8),write(' '),afc(C9),write('|'), nl,
    write('    -------'), nl,
    nl.

% Affiche les coordonnées du plateau
display_coords([C1,C2,C3,C4,C5,C6,C7,C8,C9]):-
    write('    |'),afc(C1),write(' '),afc(C2),write(' '),afc(C3),write('|'),
    write('                         |0 1 2|'),nl,
    write('    |'),afc(C4),write(' '),afc(C5),write(' '),afc(C6),write('|'),
    write('     Coordonnees     --> |3 4 5|'),nl,
    write('    |'),afc(C7),write(' '),afc(C8),write(' '),afc(C9),write('|'),
    write('                         |6 7 8|'),nl,
    !.

% Affecte les cases en fonction de leur nature
afc(X)   :- var(X), write('-').
afc(0)   :- write(' ').
afc(-1)  :- write('#').
afc(1)   :- write('o').
afc(2)   :- write('x').

% Welcome prompt
welcome :-
    nl,
    writeln('Projet Force 3'),
    writeln('Créé dans le cadre du cours IA41 (UTBM)'), nl,
    writeln('J  vs. IA :\t play.'),
    writeln('IA vs. IA :\t game_ia.'),
    nl.

:- init_ui.
