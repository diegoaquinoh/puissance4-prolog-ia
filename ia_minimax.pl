:- module(ia_minimax, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

ia(Board, Col, Player) :-
    Depth = 4,
    minimax(Board, Player, Depth, _Score, BestCol),
    (   BestCol =:= -1
    ->  findall(C, legal_move(Board, C), [Col|_])
    ;   Col = BestCol
    ),
    !.

board_full(Board) :-
    match_nul(Board).

minimax(Board, _Player, Depth, Score, -1) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board(Board, Score),
    !.

minimax(Board, Player, Depth, BestScore, BestCol) :-
    Depth > 0,
    Depth1 is Depth - 1,
    findall(Score-Col,
            (   legal_move(Board, Col),
                play_move(Board, Col, NextBoard, Player),
                change_player(Player, NextPlayer),
                minimax(NextBoard, NextPlayer, Depth1, Score, _)
            ),
            MovesScores),
    (   Player = '\U0001F534'
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

eval_board(Board, Score) :-
    (   win_player(Board, '\U0001F534')
    ->  Score = 100000
    ;   win_player(Board, '\U0001F7E1')
    ->  Score = -100000
    ;   heuristic(Board, Score)
    ).

win_player(Board, Player) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

heuristic(Board, Score) :-
    findall(Wx,
            (   any_cell(Board, '\U0001F534', Col, Row),
                cell_weight(Row, Col, Wx)
            ),
            Lx),
    sum_list(Lx, SX),
    findall(Wo,
            (   any_cell(Board, '\U0001F7E1', Col2, Row2),
                cell_weight(Row2, Col2, Wo)
            ),
            Lo),
    sum_list(Lo, SO),
    Score is SX - SO.

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
