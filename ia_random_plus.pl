:- module(ia_random_plus, [ia/3]).

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(game).

ia(Board, Col, Player) :-
    (   find_winning_move(Board, Player, WinCol)
    ->  Col = WinCol
    ;   change_player(Player, Opponent),
        find_winning_move(Board, Opponent, BlockCol)
    ->  Col = BlockCol
    ;   ia_random(Board, Col)
    ).

find_winning_move(Board, Player, Col) :-
    between(0, 6, Col),
    nth0(Col, Board, Column),
    length(Column, H),
    H < 6,
    play_move(Board, Col, TestBoard, Player),
    nth0(Col, TestBoard, ColumnAfter),
    length(ColumnAfter, NewH),
    Row is NewH - 1,
    test_win(TestBoard, Player, Col, Row),
    !.

ia_random(Board, Col) :-
    repeat,
    random_between(0, 6, Col),
    nth0(Col, Board, Column),
    length(Column, H),
    H < 6,
    !.
