:- module(profiling, [
    profile_minimax_nodes/4,
    profile_alphabeta_nodes/4,
    compare_node_efficiency/3
]).

:- use_module(library(lists)).
:- use_module(game).

% ===== COMPTEUR DE NŒUDS POUR MINIMAX =====

% Version instrumentée de minimax qui compte les nœuds
minimax_counted(Board, _Player, Depth, Score, -1, Nodes) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board(Board, Score),
    Nodes = 1,  % Nœud terminal
    !.

minimax_counted(Board, Player, Depth, BestScore, BestCol, TotalNodes) :-
    Depth > 0,
    Depth1 is Depth - 1,
    findall(result(Score, Col, Nodes),
            (   legal_move(Board, Col),
                play_move(Board, Col, NextBoard, Player),
                change_player(Player, NextPlayer),
                minimax_counted(NextBoard, NextPlayer, Depth1, Score, _, Nodes)
            ),
            Results),
    
    % Compter tous les nœuds enfants + ce nœud
    sum_nodes(Results, ChildNodes),
    TotalNodes is ChildNodes + 1,
    
    % Extraire meilleur coup
    (   Player = '\U0001F534'
    ->  best_max_result(Results, BestScore, BestCol)
    ;   best_min_result(Results, BestScore, BestCol)
    ).

sum_nodes([], 0).
sum_nodes([result(_, _, N)|Rest], Total) :-
    sum_nodes(Rest, RestTotal),
    Total is RestTotal + N.

best_max_result([result(S, C, _)], S, C).
best_max_result([result(S, C, _)|Rest], BestScore, BestCol) :-
    best_max_result(Rest, CurScore, CurCol),
    (   S > CurScore
    ->  BestScore = S, BestCol = C
    ;   BestScore = CurScore, BestCol = CurCol
    ).

best_min_result([result(S, C, _)], S, C).
best_min_result([result(S, C, _)|Rest], BestScore, BestCol) :-
    best_min_result(Rest, CurScore, CurCol),
    (   S < CurScore
    ->  BestScore = S, BestCol = C
    ;   BestScore = CurScore, BestCol = CurCol
    ).

board_full(Board) :- match_nul(Board).

eval_board(Board, Score) :-
    (   win_player(Board, '\U0001F534')
    ->  Score = 100000
    ;   win_player(Board, '\U0001F7E1')
    ->  Score = -100000
    ;   Score = 0  % Heuristique simple pour profiling
    ).

win_player(Board, Player) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

% ===== COMPTEUR DE NŒUDS POUR ALPHA-BETA =====

alphabeta_counted(Board, _Player, Depth, _Alpha, _Beta, Score, -1, Nodes) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board(Board, Score),
    Nodes = 1,
    !.

alphabeta_counted(Board, Player, Depth, Alpha, Beta, BestScore, BestCol, TotalNodes) :-
    Depth > 0,
    findall(C, legal_move(Board, C), Moves),
    % IMPORTANT : ordering (centre d'abord) comme dans ia_alphabeta.pl
    order_moves(Moves, OrderedMoves),
    (   Player = '\U0001F534'
    ->  alphabeta_max_counted(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol, ChildNodes)
    ;   alphabeta_min_counted(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol, ChildNodes)
    ),
    TotalNodes is ChildNodes + 1.

alphabeta_max_counted(_Board, _Player, _Depth, Alpha, _Beta, [], Alpha, -1, 0).

alphabeta_max_counted(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol, TotalNodes) :-
    play_move(Board, Col, NextBoard, Player),
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    alphabeta_counted(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _, ThisNodes),
    (   Score >= Beta
    ->  BestScore = Score, BestCol = Col, TotalNodes = ThisNodes
    ;   Score > Alpha
    ->  alphabeta_max_counted(Board, Player, Depth, Score, Beta, Rest, RestScore, RestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes,
        (   RestScore > Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   alphabeta_max_counted(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes
    ).

alphabeta_min_counted(_Board, _Player, _Depth, _Alpha, Beta, [], Beta, -1, 0).

alphabeta_min_counted(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol, TotalNodes) :-
    play_move(Board, Col, NextBoard, Player),
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    alphabeta_counted(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _, ThisNodes),
    (   Score =< Alpha
    ->  BestScore = Score, BestCol = Col, TotalNodes = ThisNodes
    ;   Score < Beta
    ->  alphabeta_min_counted(Board, Player, Depth, Alpha, Score, Rest, RestScore, RestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes,
        (   RestScore < Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   alphabeta_min_counted(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes
    ).

% ===== API PUBLIQUE =====

% Order moves (identique à ia_alphabeta.pl)
order_moves(Moves, Ordered) :-
    sort_by_center_distance(Moves, Ordered).

sort_by_center_distance(Moves, Sorted) :-
    maplist(add_distance, Moves, WithDist),
    keysort(WithDist, SortedWithDist),
    pairs_values(SortedWithDist, Sorted).

add_distance(Col, Dist-Col) :-
    Dist is abs(Col - 3).

pairs_values([], []).
pairs_values([_-V|T], [V|VT]) :-
    pairs_values(T, VT).

profile_minimax_nodes(Board, Player, Depth, Nodes) :-
    minimax_counted(Board, Player, Depth, _Score, _Col, Nodes).

profile_alphabeta_nodes(Board, Player, Depth, Nodes) :-
    Alpha = -1000000,
    Beta = 1000000,
    alphabeta_counted(Board, Player, Depth, Alpha, Beta, _Score, _Col, Nodes).

compare_node_efficiency(Board, Player, Depth) :-
    format('~n=== Comparaison efficacité (profondeur ~w) ===~n', [Depth]),
    
    format('Comptage minimax...~n'),
    get_time(Start1),
    profile_minimax_nodes(Board, Player, Depth, MinimaxNodes),
    get_time(End1),
    Time1 is (End1 - Start1) * 1000,
    format('  Nœuds explorés : ~w (~3f ms)~n', [MinimaxNodes, Time1]),
    
    format('Comptage alpha-beta...~n'),
    get_time(Start2),
    profile_alphabeta_nodes(Board, Player, Depth, AlphabetaNodes),
    get_time(End2),
    Time2 is (End2 - Start2) * 1000,
    format('  Nœuds explorés : ~w (~3f ms)~n', [AlphabetaNodes, Time2]),
    
    ReductionFactor is MinimaxNodes / AlphabetaNodes,
    PrunedPercent is ((MinimaxNodes - AlphabetaNodes) / MinimaxNodes) * 100,
    
    format('~n=> Alpha-beta explore ~2fx moins de nœuds~n', [ReductionFactor]),
    format('=> ~2f%% de nœuds élagués (pruning)~n', [PrunedPercent]).
