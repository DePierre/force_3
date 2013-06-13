:- module(mod_regles, [win/2, taquin_for/2, taquin2_for/3, get_neighbour/2,
                      get_opposed/2, row/3, column/3, diagonal/3, nbp/3,
                      deplacement/3]).

% Trois pions alignés sur une ligne
win(JR, [JR,JR,JR,_,_,_,_,_,_]) :- JR \= 0, !.
win(JR, [_,_,_,JR,JR,JR,_,_,_]) :- JR \= 0, !.
win(JR, [_,_,_,_,_,_,JR,JR,JR]) :- JR \= 0, !.

% Trois pions alignés sur une colonne
win(JR, [JR,_,_,JR,_,_,JR,_,_]) :- JR \= 0, !.
win(JR, [_,JR,_,_,JR,_,_,JR,_]) :- JR \= 0, !.
win(JR, [_,_,JR,_,_,JR,_,_,JR]) :- JR \= 0, !.

% Trois pions alignés sur une diagonale
win(JR, [JR,_,_,_,JR,_,_,_,JR]) :- JR \= 0, !.
win(JR, [_,_,JR,_,JR,_,JR,_,_]) :- JR \= 0.

% La position du taquin permet de savoir quelles cases
% peuvent être déplacées
taquin_for(0, 1).
taquin_for(0, 3).
taquin_for(1, 0).
taquin_for(1, 2).
taquin_for(1, 4).
taquin_for(2, 1).
taquin_for(2, 5).
taquin_for(3, 0).
taquin_for(3, 4).
taquin_for(3, 6).
taquin_for(4, 1).
taquin_for(4, 3).
taquin_for(4, 5).
taquin_for(4, 7).
taquin_for(5, 2).
taquin_for(5, 4).
taquin_for(5, 8).
taquin_for(6, 3).
taquin_for(6, 7).
taquin_for(7, 4).
taquin_for(7, 6).
taquin_for(7, 8).
taquin_for(8, 5).
taquin_for(8, 7).

% Idem lorsque deux cases sont déplaçables deux à deux
taquin2_for(0, 1, 2).
taquin2_for(0, 3, 6).
taquin2_for(1, 4, 7).
taquin2_for(2, 1, 0).
taquin2_for(2, 5, 8).
taquin2_for(3, 4, 5).
taquin2_for(5, 4, 3).
taquin2_for(6, 3, 0).
taquin2_for(6, 7, 8).
taquin2_for(7, 4, 1).
taquin2_for(8, 5, 2).
taquin2_for(8, 7, 6).

% Retourne les cases adjacentes
get_neighbour(0,1).
get_neighbour(0,3).
get_neighbour(1,0).
get_neighbour(1,2).
get_neighbour(1,4).
get_neighbour(2,1).
get_neighbour(2,5).
get_neighbour(3,0).
get_neighbour(3,4).
get_neighbour(3,6).
get_neighbour(4,1).
get_neighbour(4,3).
get_neighbour(4,5).
get_neighbour(4,7).
get_neighbour(5,2).
get_neighbour(5,4).
get_neighbour(5,8).
get_neighbour(6,3).
get_neighbour(6,7).
get_neighbour(7,4).
get_neighbour(7,6).
get_neighbour(7,8).
get_neighbour(8,5).
get_neighbour(8,7).

% Retourne la case opposée
get_opposed(0, 6).
get_opposed(1, 7).
get_opposed(2, 8).
get_opposed(3, 5).
get_opposed(5, 3).
get_opposed(6, 0).
get_opposed(7, 1).
get_opposed(8, 2).

% Unifie la séquence [E1, E2, E3] avec la Ième ligne du plateau PL
row(PL, I, [E1, E2, E3]) :-
    I1 is (I - 1) * 3, nth0(I1, PL, E1),
    I2 is 3 * I - 2, nth0(I2, PL, E2),
    I3 is 3 * I - 1, nth0(I3, PL, E3).

% Unifie la séquence [E1, E2, E3] avec la Jème colonne du plateau PL
column(PL, J, [E1, E2, E3]) :-
    nth1(J, PL, E1),
    I2 is J + 3, nth1(I2, PL, E2),
    I3 is J + 6, nth1(I3, PL, E3).

% Unifie la séquence [E1, E2, E3] avec la Nème diagonale du plateau PL
diagonal(PL, 1, [E1,E2,E3]) :-
    !, nth1(1, PL, E1),
    nth1(5, PL, E2),
    nth1(9, PL, E3).

diagonal(PL, 2, [E1,E2,E3]) :-
    nth1(3, PL, E1),
    nth1(5, PL, E2),
    nth1(7, PL, E3).


% Retourne le nombre de pions NB du joueur JR sur le plateau PL
nbp(JR, PL, NB) :-
    sublist(=(JR), PL, L),
    length(L, NB).

% Pose d'un pion, au maximum 3 pions placés
deplacement(JR, [0|R], [-1, 0, 0]) :-
    nbp(JR, [0|R], N), N < 3.

deplacement(JR, [C0, 0|R], [-1, 1, 0]) :-
    nbp(JR, [C0, 0|R], N), N < 3.

deplacement(JR, [C0, C1, 0|R], [-1, 2, 0]) :-
    nbp(JR, [C0, C1, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, 0|R], [-1, 3, 0]) :-
    nbp(JR, [C0, C1, C2, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, C3, 0|R], [-1, 4, 0]) :-
    nbp(JR, [C0, C1, C2, C3, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, C3, C4, 0|R], [-1, 5, 0]) :-
    nbp(JR,[C0, C1, C2, C3, C4, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, C3, C4, C5, 0|R], [-1, 6, 0]) :-
    nbp(JR, [C0, C1, C2, C3, C4, C5, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, C3, C4, C5, C6, 0|R], [-1, 7, 0]) :-
    nbp(JR, [C0, C1, C2, C3, C4, C5, C6, 0|R], N), N < 3.

deplacement(JR, [C0, C1, C2, C3, C4, C5, C6, C7, 0|R], [-1, 8, 0]) :-
    nbp(JR, [C0, C1, C2, C3, C4, C5, C6, C7, 0|R], N), N < 3.

% Déplacement d'un pion déjà posé sur le plateau sur l'une des 9 cases.
deplacement(JR, PL, [CA, CS, 1]) :-
    !, get_neighbour(CA, CS),
    nth0(CA, PL, JR),
    nth0(CS, PL, 0).

% Déplacement d'une case CA vers CS
deplacement(_, PL, [CA, CS, 2]) :-
    !, taquin_for(CA, CS),
    nth0(CA, PL, -1).

% Déplacement de deux cases
deplacement(_, PL, [C1, CI, C2, 3]) :-
    taquin2_for(C1, CI, C2),
    nth0(C1, PL, -1).
