:- dynamic board/1.
:- use_module(library(random)).
:- use_module(library(readutil)).

% Point d entrée : lance une partie avec l IA aléatoire en 'x'
init :-
    retractall(board(_)),
    Board = [[], [], [], [], [], [], []],
    assert(board(Board)),
    display_board,
    play('x').

play(Player) :-
    board(Board),
    (   match_nul(Board)
    ->  writeln('Match nul !')
    ;   format('Tour de ~w~n', [Player]),
        (   Player = 'x'
        ->  ia(Board, Move, Player)
        ;   human_move(Board, Move)
        ),
        play_move(Board, Move, NewBoard, Player),
        apply_board(Board, NewBoard),
        nth0(Move, NewBoard, ColumnAfter),
        length(ColumnAfter, H),
        Row is H - 1,
        display_board,
        (   test_win(NewBoard, Player, Move, Row)
        ->  format('~w gagne !~n', [Player])
        ;   change_player(Player, NextPlayer),
            play(NextPlayer)
        )
    ).

change_player('x', 'o').
change_player('o', 'x').

display_row(Board, Row) :-
    display_row_cols(Board, Row, 0),
    nl.

display_row_cols(Board, Row, Col) :-
    Col =< 6,
    nth0(Col, Board, Column),
    (   nth0(Row, Column, Cell)
    ->  print_colored_cell(Cell)
    ;   write('_')
    ),
    write(' '),
    NextCol is Col + 1,
    display_row_cols(Board, Row, NextCol).
display_row_cols(_, _, Col) :-
    Col > 6.

print_colored_cell('x') :-
    write('\e[33mx\e[0m').
print_colored_cell('o') :-
    write('\e[31mo\e[0m').
print_colored_cell(Cell) :-
    write(Cell).

display_board :-
    board(Board),
    forall(between(0, 5, R0),
           (   Row is 5 - R0,
               display_row(Board, Row)
           )),
    writeln('0 1 2 3 4 5 6').

% IA aléatoire : choisit une colonne non pleine au hasard
ia(Board, Col, _) :-
    repeat,
    random_between(0, 6, Col),
    nth0(Col, Board, Column),
    length(Column, H),
    H < 6,
    !.

play_move(Board, ColIndex, NewBoard, Player) :-
    nth0(ColIndex, Board, Column),
    length(Column, H),
    H < 6,
    append(Column, [Player], NewColumn),
    replace_nth0(ColIndex, Board, NewColumn, NewBoard).

replace_nth0(0, [_|T], X, [X|T]).
replace_nth0(I, [H|T], X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace_nth0(I1, T, X, R).

apply_board(Board, NewBoard) :-
    retract(board(Board)),
    assert(board(NewBoard)).

test_win(Board, Player, Col, Row) :-
    (   check_direction(Board, Player, Col, Row, 1, 0)     % horizontal
    ;   check_direction(Board, Player, Col, Row, 0, 1)     % vertical
    ;   check_direction(Board, Player, Col, Row, 1, 1)     % diagonale /
    ;   check_direction(Board, Player, Col, Row, 1, -1)    % diagonale \
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

