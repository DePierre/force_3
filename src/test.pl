% Tests de min/max
leaf([_, []]).
node(X) :- leaf(X).
node([_, L]) :- maplist(node, L).

node_addchild([Data, Children], Child, [Data, NewChildren]) :-
    node(Child),
    append(Children, [Child], NewChildren).

node_setvalue([Data, _], Value):-
    Data is Value.

print_tree([Data, Children]) :-
    node([Data, Children]),
    value([Data, Children], V),
    writef("%w %w (value= %w)", [Data, Children, V]), nl,
    maplist(print_tree, Children).

value([Data, _], Data).


:- dynamic spy/0.  % debug calls to alpha_beta
:- assert(spy).    % Comment out stop spy.

neg(X, -X).

minmax(0, Node,  Value) :-
    value(Node, Value).
minmax(_Depth, Node, Value) :-
    leaf(Node),
    value(Node, Value),
    writef("%w %w \n",[Node, Value]).

minmax(Depth, [Data, Children], Value) :-
    Depth1 is Depth -1,
    maplist(minmax(Depth1), Children, Z),
    maplist(neg, Z, T),
    max_list(T, Value),
    Data is Value,
    writef("%w Depth : %w, Value : %w\n",[Children, Depth, Value]).


%:-  B1 =[_,[[12,[]], [10,[]], [3,[]]]],
%    B2 =[_,[[5,[]], [8,[]], [10,[]]]],
%    B3 =[_,[[11,[]], [2,[]], [12,[]]]],
%    A = [_,[B1,B2,B3]],

%    minmax(3, A, V), writeln(V).
:-  D1 = [_, [[1, []], [1, []]]],
    D2 = [_, [[0, []], [3, []]]],
    D3 = [_, [[7, []], [4, []], [8, []]]],
    D4 = [_, [[4, []], [19, []]]],
    D5 = [_, [[-2, []],[1, []]]],
    D6 = [_, [[-7, []],[4, []]]],
    D7 = [_, [[0, []]]],
    D8 = [_, [[3, []], [-4, []], [4, []]]],

    C1 = [_, [D1, D2]],
    C2 = [_, [D3]],
    C3 = [_, [D4, D5]],
    C4 = [_, [D6, D7]],
    C5 = [_, [D8]],

    B1 = [_, [C1, C2, C3]],
    B2 = [_, [C4, C5]],

    A = [_, [B1, B2]],
    minmax(5, A, V),
    writeln(V).
