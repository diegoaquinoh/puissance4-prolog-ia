:- module(ia_minimax, [ia/3, set_depth/1, set_depth_for/2]).

:- use_module(library(lists)).
:- use_module(game).

:- dynamic minimax_depth_default/1.
:- dynamic minimax_depth/2. % minimax_depth(PlayerSymbol, Depth)
minimax_depth_default(4).

% règle la profondeur par défaut (utilisée si aucune profondeur spécifique au joueur n est définie)
set_depth(D) :-
    integer(D), D >= 1,
    retractall(minimax_depth_default(_)),
    assertz(minimax_depth_default(D)).

% règle la profondeur pour un joueur (symbole Unicode '\U0001F534' ou '\U0001F7E1')
set_depth_for(Player, D) :-
    (Player = '\U0001F534' ; Player = '\U0001F7E1'),
    integer(D), D >= 1,
    retractall(minimax_depth(Player, _)),
    assertz(minimax_depth(Player, D)).

ia(Board, Col, Player) :-
    (   minimax_depth(Player, Depth)
    ->  true
    ;   (minimax_depth_default(Depth) -> true ; Depth = 4)
    ),
    minimax(Board, Player, Player, Depth, _Score, BestCol),
    (   BestCol =:= -1
    ->  findall(C, legal_move(Board, C), [Col|_])
    ;   Col = BestCol
    ),
    !.

board_full(Board) :-
    match_nul(Board).

minimax(Board, RootPlayer, _CurrentPlayer, Depth, Score, -1) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board(Board, RootPlayer, Score),
    !.

minimax(Board, RootPlayer, CurrentPlayer, Depth, BestScore, BestCol) :-
    Depth > 0,
    Depth1 is Depth - 1,
    findall(Score-Col,
        (   legal_move(Board, Col),
            play_move(Board, Col, NextBoard, CurrentPlayer),
            change_player(CurrentPlayer, NextPlayer),
            minimax(NextBoard, RootPlayer, NextPlayer, Depth1, Score, _)
        ),
        MovesScores),
    (   CurrentPlayer = RootPlayer
    ->  best_max(MovesScores, BestScore, BestCol)
    ;   best_min(MovesScores, BestScore, BestCol)
    ).

best_max([S-C], S, C).
best_max([S-C|Rest], BestScore, BestCol) :-
    best_max(Rest, CurScore, CurCol),
    (   S > CurScore
    ->  BestScore = S, BestCol = C
    ;   BestScore = CurScore, BestCol = CurCol
    ).

best_min([S-C], S, C).
best_min([S-C|Rest], BestScore, BestCol) :-
    best_min(Rest, CurScore, CurCol),
    (   S < CurScore
    ->  BestScore = S, BestCol = C
    ;   BestScore = CurScore, BestCol = CurCol
    ).

eval_board(Board, RootPlayer, Score) :-
    (   win_player(Board, RootPlayer)
    ->  Score = 100000
    ;   change_player(RootPlayer, Opp),
        win_player(Board, Opp)
    ->  Score = -100000
    ;   heuristic(Board, RootPlayer, Score)
    ).

win_player(Board, Player) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

heuristic(Board, RootPlayer, Score) :-
    findall(W1,
        (   any_cell(Board, RootPlayer, Col, Row),
            cell_weight(Row, Col, W1)
        ),
        L1),
    sum_list(L1, S1),

    change_player(RootPlayer, Opp),
    findall(W2,
        (   any_cell(Board, Opp, Col2, Row2),
            cell_weight(Row2, Col2, W2)
        ),
        L2),
    sum_list(L2, S2),

    Score is S1 - S2.

any_cell(Board, Player, Col, Row) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player).

cell_weight_matrix([
    [3, 4, 5, 7, 5, 4, 3],
    [4, 6, 8, 10, 8, 6, 4],
    [5, 8, 11, 14, 11, 8, 5],
    [5, 8, 11, 14, 11, 8, 5],
    [4, 6, 8, 10, 8, 6, 4],
    [3, 4, 5, 7, 5, 4, 3]
]).

cell_weight(Row, Col, W) :-
    cell_weight_matrix(Matrix),
    nth0(Row, Matrix, Line),
    nth0(Col, Line, W).