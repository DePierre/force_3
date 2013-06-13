:- module(mod_eval,[eval_nb_move/3,eval_bord/2, alpha_beta/7]).
:- use_module('mod_regles.pl').
:- use_module('mod_jeu.pl').
:- use_module(library(lists)).


% Le module mod_eval permet de calculer la valeur optimale pour un joueur



% eval_nb_move(+J, +P, ?NbM)
% retourne le nombre de coup NbC du joueur J pour le plateau P
eval_nb_coups(J,P,NbM) :- findall(_,deplacement(J,P,_), ListeMove), length(ListeMove,NbM).



% eval_bord(+P, ?Value)
% Permet de déterminer l'avantage d'un des deux joueurs. Si la valeur de retour est positif, alors le joueur 1 à l'avantage. Et vise versa.
% La valeur maximale est abs(100).
eval_bord(P, 100)   :- win(1, P), !. % Victoire du joueur 1
eval_bord(P, -100)  :- win(2, P), !. % Victoire du joueur 2

eval_bord(P, V) :-
    eval_value(1, P, ValueJ1),
    eval_value(2, P, ValueJ2),
    V is ValueJ1 - ValueJ2.


% eval_value(+J, +P, ?Value)
% Retourne la valeur pour le joueur J
eval_value(J, P, Value) :-
    maplist(row(P), [1,2,3], Rows),
    maplist(compute_points(J), Rows, PRows),
    sumlist(PRows, PtsRows),

    maplist(column(P), [1,2,3], Cols),
    maplist(compute_points(J), Cols, PCols),
    sumlist(PCols, PtsCols),

    diagonal(P, 1, Prof1), compute_points(J, Prof1, PtsProf1),
    diagonal(P, 2, Prof2), compute_points(J, Prof2, PtsProf2),

    Value is PtsRows + PtsCols + PtsProf1 + PtsProf2.


% compute_points(+J, +List, ?Score)
% Distribut un score en fonction du nombre de pionts allignés.
compute_points(J, List, Score) :-
     include(=:=(JR), List, Filtered),
     length(Filtered, Len),
     scores(Len, Score).


% scores(+NbPions, ?Scores)
% Retourne le score associé au nombre de pionts alignés.
scores(0,0).
scores(1,1).
scores(2,3).


% alpha_beta(+J, +Depth, +P, +Alpha, +Beta, ?Move, ?Value)
% Algorithme d'élagage utilisant la méthode alpha-beta. Depth est la profondeur de recherche avec Value la valeur du plateau lorsque le coup est joué.
alpha_beta(_J,0,P,_Alpha,_Beta,_Move,Value) :-
    % On s arrête lorsqu on arrive à une depth égale à 0.
   eval_bord(P,Value),!.

alpha_beta(J,Depth,P,Alpha,Beta,Move,Value) :-
   Prof > 0,
   findall(X,move(J,P,X,_),Moves),
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   Depth1 is Depth-1,
   find_best(J,Moves,P,Depth1,Alpha1,Beta1,nil,(Move,Value)),!.


% find_best(+J,+Moves,+P,+Depth,+Alpha,+Beta,+R,?BestMove)
% Retourne le meilleur coup à jouer.
find_best(J,[Move|Moves],P,Depth,Alpha,Beta,R,BestMove) :-
   mouv(J,P,Move,NP),
   autre_joueur(J,OtherJR),
   alpha_beta(OtherJR,Depth,NP,Alpha,Beta,_OtherCoup,Value),
   Value1 is -Value,
   elague(J,Move,Value1,Depth,Alpha,Beta,Moves,P,R,BestMove),!.
find_best(_J,[],_P,_Depth,Alpha,_Beta,Move,(Move,Alpha)).


% elague(+J,+Move,+Value,+Depth,+Alpha,+Beta,+Moves,+P,+_R,+BestMove)
% Permet d'élaguer l'abre de recherche en fonction d'alpha et beta.
% La recherche s'arrête dans une branche lorsqu'elle sort des bornes.
elague(J,Move,Value,Depth,Alpha,Beta,Moves,P,_R,BestMove) :-
   Alpha < Value, Value < Beta, !,
   find_best(J,Moves,P,Depth,Value,Beta,Move,BestMove),!.
elague(J,_Move,Value,Depth,Alpha,Beta,Moves,P,R,BestMove) :-
   Value =< Alpha, !,
   find_best(J,Moves,P,Depth,Alpha,Beta,R,BestMove),!.
elague(_J,Move,Value,_Depth,_Alpha,Beta,_Moves,_P,_R,(Move,Value)) :-
   Value >= Beta, !.

