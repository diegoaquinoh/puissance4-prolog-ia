:- module(game, [
    init_game/1,
    play/2,
    play_move/4,
    test_win/4,
    match_nul/1,
    legal_move/2,
    change_player/2,
    display_board/1,
    human_move/2,
    get_cell/4,
    play_hvh/0
]).

:- use_module(library(lists)).
:- use_module(library(readutil)).

change_player('\U0001F534', '\U0001F7E1').
change_player('\U0001F7E1', '\U0001F534').

init_game(Board) :-
    Board = [[], [], [], [], [], [], []].

display_row(Board, Row) :-
    display_row_cols(Board, Row, 0),
    nl.

display_row_cols(Board, Row, Col) :-
    Col =< 6,
    nth0(Col, Board, Column),
    (   nth0(Row, Column, Cell)
    ->  write(Cell)
    ;   write('\u26AA')
    ),
    write(' '),
    NextCol is Col + 1,
    display_row_cols(Board, Row, NextCol).
display_row_cols(_, _, Col) :-
    Col > 6.

display_board(Board) :-
    forall(between(0, 5, R0),
           (   Row is 5 - R0,
               display_row(Board, Row)
           )),
    writeln('\u0030\uFE0F\u20E3  \u0031\uFE0F\u20E3  \u0032\uFE0F\u20E3  \u0033\uFE0F\u20E3  \u0034\uFE0F\u20E3  \u0035\uFE0F\u20E3  \u0036\uFE0F\u20E3').

legal_move(Board, Col) :-
    between(0, 6, Col),
    nth0(Col, Board, Column),
    length(Column, H),
    H < 6.

play_move(Board, ColIndex, NewBoard, Player) :-
    nth0(ColIndex, Board, Column),
    length(Column, H),
    H < 6,
    append(Column, [Player], NewColumn),
    replace_nth0(ColIndex, Board, NewColumn, NewBoard).

%replace_nth0(Index, List, NewValue, NewList)
replace_nth0(0, [_|T], X, [X|T]).
replace_nth0(I, [H|T], X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace_nth0(I1, T, X, R).

test_win(Board, Player, Col, Row) :-
    (   check_direction(Board, Player, Col, Row, 1, 0)
    ;   check_direction(Board, Player, Col, Row, 0, 1)
    ;   check_direction(Board, Player, Col, Row, 1, 1)
    ;   check_direction(Board, Player, Col, Row, 1, -1)
    ).

check_direction(Board, Player, Col, Row, DX, DY) :-
    count_direction(Board, Player, Col, Row, DX, DY, Count1),
    OppDX is -DX,
    OppDY is -DY,
    count_direction(Board, Player, Col, Row, OppDX, OppDY, Count2),
    Count is Count1 + Count2 - 1,
    Count >= 4.

count_direction(Board, Player, Col, Row, DX, DY, Count) :-
    NextCol is Col + DX,
    NextRow is Row + DY,
    (   get_cell(Board, NextCol, NextRow, Player)
    ->  count_direction(Board, Player, NextCol, NextRow, DX, DY, NextCount),
        Count is NextCount + 1
    ;   Count = 1
    ).

get_cell(Board, Col, Row, Cell) :-
    Col >= 0, Col < 7,
    nth0(Col, Board, Column),
    Row >= 0, Row < 6,
    nth0(Row, Column, Cell).

human_move(Board, Col) :-
    repeat,
    write('Colonne (0-6) : '),
    read_line_to_string(user_input, Line0),
    normalize_space(atom(Atom), Line0),
    (   atom_number(Atom, Col),
        integer(Col),
        between(0, 6, Col),
        nth0(Col, Board, Column),
        length(Column, H),
        H < 6
    ->  true
    ;   writeln('Coup invalide, réessayez.'),
        fail
    ).

match_nul(Board) :-
    \+ (member(Column, Board), length(Column, L), L < 6).

play(IAModule, FirstPlayer) :-
    init_game(Board),
    display_board(Board),
    play_loop(Board, FirstPlayer, IAModule).

play_loop(Board, Player, IAModule) :-
    (   match_nul(Board)
    ->  writeln('Match nul !')
    ;   format('Tour de ~w~n', [Player]),
        (   Player = '\U0001F534'
        ->  IAModule:ia(Board, Move, Player)
        ;   human_move(Board, Move)
        ),
        play_move(Board, Move, NewBoard, Player),
        nth0(Move, NewBoard, ColumnAfter),
        length(ColumnAfter, H),
        Row is H - 1,
        display_board(NewBoard),
        (   test_win(NewBoard, Player, Move, Row)
        ->  format('~w gagne !~n', [Player])
        ;   change_player(Player, NextPlayer),
            play_loop(NewBoard, NextPlayer, IAModule)
        )
    ).

play_hvh :-
    init_game(Board),
    display_board(Board),
    play_loop_hvh(Board, '\U0001F534').

play_loop_hvh(Board, Player) :-
    (   match_nul(Board)
    ->  writeln('Match nul !')
    ;   format('Tour de ~w~n', [Player]),
        human_move(Board, Move),
        play_move(Board, Move, NewBoard, Player),
        nth0(Move, NewBoard, ColumnAfter),
        length(ColumnAfter, H),
        Row is H - 1,
        display_board(NewBoard),
        (   test_win(NewBoard, Player, Move, Row)
        ->  format('~w gagne !~n', [Player])
        ;   change_player(Player, NextPlayer),
            play_loop_hvh(NewBoard, NextPlayer)
        )
    ).
