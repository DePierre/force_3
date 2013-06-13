:- module(mod_eval,[eval_nb_coups/3,eval_plateau/2, alpha_beta/7]).
:- use_module('mod_regles.pl').
:- use_module('mod_jeu.pl').
:- use_module(library(lists)).

/** <module> Evaluation

     Ce module permet d'évaluer la "valeur" d'un plateau donné. Cette valeur
     indique pour quel joueur une situation donnée est la plus favorable.

     @author Romain Boissat, Frédéric Rechtenstein, Maxime Ripard
     @license GPL
*/


/**  eval_nb_coup(+JR,+PL,?NbC) is det

    Vrai si NbC est le nombre de coups que peut faire le joueur JR sur le
    plateau de jeuPL.
*/
eval_nb_coups(JR,PL,NbC) :- findall(_,coup(JR,PL,_), Bag), length(Bag,NbC).

/**  eval_plateau(+Pl, ?Valeur) is det

   Determine la "valeur" du plateau Pl.
   Si la valeur est positive, le joueur 1 est avantagé par rapport au
   joueur 2, et vice versa.
   Valeur = 100 quand le joueur 1 gagne et -100 quand le joueur 2 gagne.
*/
eval_plateau(PL, 100)   :- win(1, PL), !. % JR 1 gagne
eval_plateau(PL, -100)  :- win(2, PL), !. % JR 2 gagne

eval_plateau(PL, V) :-  % cas général
    eval_score(1, PL, ScoreJ1),
    eval_score(2, PL, ScoreJ2),
    V is ScoreJ1 - ScoreJ2.     % score du joueur 1 - score du joueur 2


/**  eval_score(+JR, +Pl, ?score) is det

    Vrai quand Score est le score du joueur JR sur le plateau PL.
    Le score est calculé selon la méthode décrite dans le rapport.
*/
eval_score(JR, PL, Score) :-
    maplist(row(PL), [1,2,3], Rows),
    maplist(compte_points(JR), Rows, PR),
    sumlist(PR, PtsRows),

    maplist(column(PL), [1,2,3], Cols),
    maplist(compte_points(JR), Cols, PC),
    sumlist(PC, PtsCols),

    diagonal(PL, 1, Prof1), compte_points(JR, Prof1, PtsProf1),
    diagonal(PL, 2, Prof2), compte_points(JR, Prof2, PtsProf2),

    Score is PtsRows + PtsCols + PtsProf1 + PtsProf2.


/**   compte_points(+JR, +Sequence, ?Points) is det

    Vrai si Points est le nombre obtenu par le joueur JR sur la liste Sequence.
    En supposant que Sequence soit une liste de case alignés (ligne, colonne
    ou diagonale).
*/
compte_points(JR, Sequence, Points) :-
     include(=:=(JR), Sequence, Filtered),
     length(Filtered, Len),
     points(Len, Points).


/**  points(+NbPions, ?Points) is det

    Vrai si le nombre de pions alignés est associé au nombre de points.
    Permet de calculer le nombre de points pour un nombre de pions alignés.
*/
points(0,0).
points(1,1).
points(2,5).


/**  alpha_beta(+JR, +Prof, +PL, +Alpha, +Beta, ?Coup, ?Valeur)

    Vrai si le coup Coup est le meilleur coup à jouer pour le joueur JR
    en partant du plateau PL.
    La détermination du meilleur coup se fait en utilisant l'algorithme
    négamax avec élégage alpha-beta. Prof est la profondeur de la recherche et
    Valeur est la "valeur" du nouveau plateau qui sera obtenu si le coup est
    joué. A utiliser avec les parametres Alpha = -infini et Beta = +infini.
*/
alpha_beta(_JR,0,PL,_Alpha,_Beta,_Coup,Valeur) :-
    % Quand la profondeur est 0 : stop ! retourner la valeur du plateau
   eval_plateau(PL,Valeur),!.
%    spy(JR,PL,Valeur).

alpha_beta(JR,Prof,PL,Alpha,Beta,Coup,Valeur) :-
   Prof > 0,
   findall(X,move(JR,PL,X,_),Coups),
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   Prof1 is Prof-1,
   cherche_meilleur(JR,Coups,PL,Prof1,Alpha1,Beta1,nil,(Coup,Valeur)),!.



/**  cherche_meilleur(+JR,+Coups,+PL,+Prof,+Alpha,+Beta,+R,?MeilleurCoup)

    Cherche le meilleur coup MeilleurCoup à jouer dans la liste des coups Coups.

    @see alpha_beta
*/
cherche_meilleur(JR,[Coup|Coups],PL,Prof,Alpha,Beta,R,MeilleurCoup) :-
   move(JR,PL,Coup,NP),
   get_opponent(JR,OtherJR),
   alpha_beta(OtherJR,Prof,NP,Alpha,Beta,_OtherCoup,Valeur),
   Valeur1 is -Valeur,
   elague(JR,Coup,Valeur1,Prof,Alpha,Beta,Coups,PL,R,MeilleurCoup),!.
cherche_meilleur(_JR,[],_PL,_Prof,Alpha,_Beta,Coup,(Coup,Alpha)).


/**  elague(+JR,+Coup,+Valeur,+Prof,+Alpha,+Beta,+Coups,+PL,_R,?MeilleurCoup)

    Prédicat d'élagage en alpha-beta. Stope la recheche dans une branche de
    l'arbre quand elle ne peut contribuer au calcul de la valeur de la dite
    branche.

    @see alpha_beta
*/
elague(JR,Coup,Valeur,Prof,Alpha,Beta,Coups,PL,_R,MeilleurCoup) :-
   Alpha < Valeur, Valeur < Beta, !,
   cherche_meilleur(JR,Coups,PL,Prof,Valeur,Beta,Coup,MeilleurCoup),!.
elague(JR,_Coup,Valeur,Prof,Alpha,Beta,Coups,PL,R,MeilleurCoup) :-
   Valeur =< Alpha, !,
   cherche_meilleur(JR,Coups,PL,Prof,Alpha,Beta,R,MeilleurCoup),!.
elague(_JR,Coup,Valeur,_Prof,_Alpha,Beta,_Coups,_PL,_R,(Coup,Valeur)) :-
   Valeur >= Beta, !.

% EOF
