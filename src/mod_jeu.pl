%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          ______                                             %
%                         /___ ___  __  __   __   ___                         %
%                        /    /  / /_/ /    /_    ___\                        %
%                       /    /__/ / \  \__ /__    ___/                        %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Projet de force3 pour l'UV IA41
% Implantation de l'algorithme MinMax
%
% Copyright (c) 2008  Romain BOISSAT, Frederic RECHTENSTEIN, Maxime RIPARD
% This program is free software ; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation ; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY ; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program ; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mod_jeu,[empty_board/1, setBoard/4, move/4, autre_joueur/2]).
:- use_module('mod_regles.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Prédicats de jeu                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialisation Plateau vide
empty_board([0,0,0,0,-1,0,0,0,0]).

% Modification du plateau
% setBoard/4 (+Value, +Case, +PlateauActuel, +PlateauSuivant)
setBoard(Value,0,[_|T],[Value|T]).
setBoard(Value,Index,[X|T],[X|Y]) :-  Index>0, Index1 is Index-1,
                                      setBoard(Value,Index1,T,Y),!.

% moveements de Jeu effectifs
% move/4 renvoie le plateau une fois le deplacement joué
% move/4 (+JR, +PL, +[CA,CS,Id_moveement], ?NouveauPL)
% Il faut encore le déterminer avec la fonction dévalutation de mod_eval.pl

% Pose pion
move(JR, PL, [-1,C,0], NPL)    :- deplacement(JR, PL, [-1,C,0]),
                                  setBoard(JR, C, PL, NPL).
% Déplacement pion
move(JR, PL, [CA,CS,1], NPL)   :- deplacement(JR, PL, [CA,CS,1]),
                                  setBoard(0, CA, PL, Temp),
                                  setBoard(JR, CS, Temp, NPL).
% Déplacement case
move(_, PL, [CA,CS,2], NPL)    :- deplacement(_, PL, [CA,CS,2]),
                                  setBoard(-1, CS, PL, Temp),
                                  nth0(CS, PL, Val),
                                  setBoard(Val, CA, Temp, NPL).
% Déplacement 2 cases
move(_, PL, [C1,C2,3], NPL) :-    get_opponent(C1,C2),
                                  move(_, PL, [C1,CI,2], Temp0),
                                  move(_, Temp0, [CI,C2,2], Temp1),
                                  setBoard(-1, C2, Temp1, NPL).

autre_joueur(1,2).
autre_joueur(2,1).

% EOF
