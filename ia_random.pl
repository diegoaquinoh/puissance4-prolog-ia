:- module(ia_random, [ia/3]).

:- use_module(library(random)).
:- use_module(library(lists)).

ia(Board, Col, _Player) :-
    repeat,
    random_between(0, 6, Col),
    nth0(Col, Board, Column),
    length(Column, H),
    H < 6,
    !.
