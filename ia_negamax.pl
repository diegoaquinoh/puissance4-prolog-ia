:- module(ia_negamax, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

ia(Board, Col, Player) :-
    Depth = 4,
    negamax(Board, Player, Depth, _Score, BestCol),
    (   BestCol =:= -1
    ->  findall(C, legal_move(Board, C), [Col|_])
    ;   Col = BestCol
    ),
    !.

board_full(Board) :-
    match_nul(Board).

% Cas terminal : profondeur 0, victoire ou plateau plein
negamax(Board, Player, Depth, Score, -1) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board_negamax(Board, Player, Score),
    !.

% Cas récursif : NegaMax(p) = max(-NegaMax(pi))
negamax(Board, Player, Depth, BestScore, BestCol) :-
    Depth > 0,
    Depth1 is Depth - 1,
    change_player(Player, NextPlayer),
    findall(Score-Col,
            (   legal_move(Board, Col),
                play_move(Board, Col, NextBoard, Player),
                negamax(NextBoard, NextPlayer, Depth1, ChildScore, _),
                Score is -ChildScore
            ),
            MovesScores),
    best_negamax(MovesScores, BestScore, BestCol).

% Sélectionne le meilleur coup (maximum)
best_negamax([S-C], S, C).
best_negamax([S-C|Rest], BestScore, BestCol) :-
    best_negamax(Rest, CurScore, CurCol),
    (   S > CurScore
    ->  BestScore = S, BestCol = C
    ;   BestScore = CurScore, BestCol = CurCol
    ).

% Évaluation symétrique : positive si favorable au joueur courant
eval_board_negamax(Board, Player, Score) :-
    (   win_player(Board, Player)
    ->  Score = 100000
    ;   change_player(Player, Opponent),
        win_player(Board, Opponent)
    ->  Score = -100000
    ;   heuristic_negamax(Board, Player, Score)
    ).

win_player(Board, Player) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

% Heuristique symétrique : positive pour le joueur courant
heuristic_negamax(Board, Player, Score) :-
    change_player(Player, Opponent),
    findall(Wx,
            (   any_cell(Board, Player, Col, Row),
                cell_weight(Row, Col, Wx)
            ),
            Lx),
    sum_list(Lx, SX),
    findall(Wo,
            (   any_cell(Board, Opponent, Col2, Row2),
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
