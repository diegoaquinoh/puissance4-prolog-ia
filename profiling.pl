:- module(profiling, [
    profile_minimax_nodes/4,
    profile_alphabeta_nodes/4,
    profile_minimax_smart_nodes/4,
    profile_alphabeta_smart_nodes/4,
    compare_node_efficiency/3,
    compare_all_algorithms/3
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

% ===== COMPTEUR DE NŒUDS POUR MINIMAX SMART =====

% Version instrumentée de minimax smart avec heuristiques avancées
minimax_smart_counted(Board, _Player, Depth, Score, -1, Nodes) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board_smart(Board, Score),
    Nodes = 1,
    !.

minimax_smart_counted(Board, Player, Depth, BestScore, BestCol, TotalNodes) :-
    Depth > 0,
    Depth1 is Depth - 1,
    findall(result(Score, Col, Nodes),
            (   legal_move(Board, Col),
                play_move(Board, Col, NextBoard, Player),
                change_player(Player, NextPlayer),
                minimax_smart_counted(NextBoard, NextPlayer, Depth1, Score, _, Nodes)
            ),
            Results),
    
    sum_nodes(Results, ChildNodes),
    TotalNodes is ChildNodes + 1,
    
    (   Player = '\U0001F534'
    ->  best_max_result(Results, BestScore, BestCol)
    ;   best_min_result(Results, BestScore, BestCol)
    ).

% ===== COMPTEUR DE NŒUDS POUR ALPHA-BETA SMART =====

alphabeta_smart_counted(Board, _Player, Depth, _Alpha, _Beta, Score, -1, Nodes) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board_smart(Board, Score),
    Nodes = 1,
    !.

alphabeta_smart_counted(Board, Player, Depth, Alpha, Beta, BestScore, BestCol, TotalNodes) :-
    Depth > 0,
    findall(C, legal_move(Board, C), Moves),
    order_moves(Moves, OrderedMoves),
    (   Player = '\U0001F534'
    ->  alphabeta_smart_max_counted(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol, ChildNodes)
    ;   alphabeta_smart_min_counted(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol, ChildNodes)
    ),
    TotalNodes is ChildNodes + 1.

alphabeta_smart_max_counted(_Board, _Player, _Depth, Alpha, _Beta, [], Alpha, -1, 0).

alphabeta_smart_max_counted(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol, TotalNodes) :-
    play_move(Board, Col, NextBoard, Player),
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    alphabeta_smart_counted(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _, ThisNodes),
    (   Score >= Beta
    ->  BestScore = Score, BestCol = Col, TotalNodes = ThisNodes
    ;   Score > Alpha
    ->  alphabeta_smart_max_counted(Board, Player, Depth, Score, Beta, Rest, RestScore, RestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes,
        (   RestScore > Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   alphabeta_smart_max_counted(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes
    ).

alphabeta_smart_min_counted(_Board, _Player, _Depth, _Alpha, Beta, [], Beta, -1, 0).

alphabeta_smart_min_counted(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol, TotalNodes) :-
    play_move(Board, Col, NextBoard, Player),
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    alphabeta_smart_counted(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _, ThisNodes),
    (   Score =< Alpha
    ->  BestScore = Score, BestCol = Col, TotalNodes = ThisNodes
    ;   Score < Beta
    ->  alphabeta_smart_min_counted(Board, Player, Depth, Alpha, Score, Rest, RestScore, RestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes,
        (   RestScore < Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   alphabeta_smart_min_counted(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol, RestNodes),
        TotalNodes is ThisNodes + RestNodes
    ).

% ===== HEURISTIQUES SMART (positional + window gravity-aware) =====

eval_board_smart(Board, Score) :-
    (   win_player(Board, '\U0001F534')
    ->  Score = 100000
    ;   win_player(Board, '\U0001F7E1')
    ->  Score = -100000
    ;   heuristic_smart(Board, Score)
    ).

heuristic_smart(Board, Score) :-
    positional_heuristic(Board, PosScore),
    window_heuristic(Board, WinScore),
    Score is PosScore * 10 + WinScore.

% Positional heuristic
positional_heuristic(Board, Score) :-
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
    [1, 2, 4, 7, 4, 2, 1],
    [2, 4, 6, 10, 6, 4, 2],
    [3, 5, 8, 13, 8, 5, 3],
    [3, 5, 8, 13, 8, 5, 3],
    [2, 4, 6, 10, 6, 4, 2],
    [1, 2, 4, 7, 4, 2, 1]
]).

cell_weight(Row, Col, W) :-
    cell_weight_matrix(Matrix),
    nth0(Row, Matrix, Line),
    nth0(Col, Line, W).

% Window heuristic (simplified - scores basic threats)
window_heuristic(Board, Score) :-
    get_column_heights(Board, Heights),
    score_all_windows(Board, Heights, Score).

get_column_heights(Board, Heights) :-
    maplist(length, Board, Heights).

score_all_windows(Board, Heights, Score) :-
    findall(S,
            (   window_with_positions(Board, Window, Positions),
                score_window_gravity(Window, Positions, Heights, S)
            ),
            Scores),
    sum_list(Scores, Score).

% Window generation (horizontal, vertical, diagonals)
window_with_positions(Board, Window, Positions) :-
    between(0, 5, Row),
    between(0, 3, StartCol),
    findall(Cell-pos(Col, Row),
            (   between(0, 3, Offset),
                Col is StartCol + Offset,
                get_cell_or_empty(Board, Col, Row, Cell)
            ),
            CellPosList),
    pairs_keys_values(CellPosList, Window, Positions).

window_with_positions(Board, Window, Positions) :-
    between(0, 6, Col),
    between(0, 2, StartRow),
    findall(Cell-pos(Col, Row),
            (   between(0, 3, Offset),
                Row is StartRow + Offset,
                get_cell_or_empty(Board, Col, Row, Cell)
            ),
            CellPosList),
    pairs_keys_values(CellPosList, Window, Positions).

window_with_positions(Board, Window, Positions) :-
    between(0, 3, StartCol),
    between(0, 2, StartRow),
    findall(Cell-pos(Col, Row),
            (   between(0, 3, Offset),
                Col is StartCol + Offset,
                Row is StartRow + Offset,
                get_cell_or_empty(Board, Col, Row, Cell)
            ),
            CellPosList),
    pairs_keys_values(CellPosList, Window, Positions).

window_with_positions(Board, Window, Positions) :-
    between(0, 3, StartCol),
    between(3, 5, StartRow),
    findall(Cell-pos(Col, Row),
            (   between(0, 3, Offset),
                Col is StartCol + Offset,
                Row is StartRow - Offset,
                get_cell_or_empty(Board, Col, Row, Cell)
            ),
            CellPosList),
    pairs_keys_values(CellPosList, Window, Positions).

get_cell_or_empty(Board, Col, Row, Cell) :-
    nth0(Col, Board, Column),
    (   nth0(Row, Column, C)
    ->  Cell = C
    ;   Cell = empty
    ).

% Gravity-aware window scoring
score_window_gravity(Window, Positions, Heights, Score) :-
    count_in_window(Window, '\U0001F534', MyCount),
    count_in_window(Window, '\U0001F7E1', OppCount),
    EmptyCount is 4 - MyCount - OppCount,
    (   EmptyCount =:= 0
    ->  window_score(MyCount, OppCount, EmptyCount, Score)
    ;   find_empty_positions(Window, Positions, EmptyPositions),
        classify_threat(EmptyPositions, Heights, ThreatType),
        window_score_gravity(MyCount, OppCount, ThreatType, Score)
    ).

% Basic window scores
window_score(4, 0, 0, 10000).
window_score(3, 0, 1, 50).
window_score(2, 0, 2, 5).
window_score(0, 3, 1, -70).
window_score(0, 2, 2, -5).
window_score(_, _, _, 0).

% Gravity-aware scores
window_score_gravity(4, 0, _, 10000) :- !.
window_score_gravity(3, 0, immediate, 100) :- !.
window_score_gravity(3, 0, near, 30) :- !.
window_score_gravity(3, 0, floating, 5) :- !.
window_score_gravity(2, 0, immediate, 20) :- !.
window_score_gravity(2, 0, near, 8) :- !.
window_score_gravity(2, 0, floating, 2) :- !.
window_score_gravity(0, 3, immediate, -120) :- !.
window_score_gravity(0, 3, near, -40) :- !.
window_score_gravity(0, 3, floating, -5) :- !.
window_score_gravity(0, 2, immediate, -20) :- !.
window_score_gravity(0, 2, near, -8) :- !.
window_score_gravity(0, 2, floating, -2) :- !.
window_score_gravity(_, _, _, 0).

find_empty_positions([], [], []).
find_empty_positions([Cell|Cells], [Pos|Positions], EmptyPos) :-
    (   Cell == empty
    ->  EmptyPos = [Pos|RestEmpty]
    ;   EmptyPos = RestEmpty
    ),
    find_empty_positions(Cells, Positions, RestEmpty).

classify_threat(EmptyPositions, Heights, ThreatType) :-
    (   any_playable_now(EmptyPositions, Heights)
    ->  ThreatType = immediate
    ;   any_near_playable(EmptyPositions, Heights)
    ->  ThreatType = near
    ;   ThreatType = floating
    ).

any_playable_now(EmptyPositions, Heights) :-
    member(pos(Col, Row), EmptyPositions),
    nth0(Col, Heights, Height),
    Row =:= Height,
    !.

any_near_playable(EmptyPositions, Heights) :-
    member(pos(Col, Row), EmptyPositions),
    nth0(Col, Heights, Height),
    Diff is Row - Height,
    Diff >= 1, Diff =< 2,
    !.

count_in_window(Window, Player, Count) :-
    include(==(Player), Window, Matches),
    length(Matches, Count).

% ===== API PUBLIQUE SMART =====

profile_minimax_smart_nodes(Board, Player, Depth, Nodes) :-
    minimax_smart_counted(Board, Player, Depth, _Score, _Col, Nodes).

profile_alphabeta_smart_nodes(Board, Player, Depth, Nodes) :-
    Alpha = -1000000,
    Beta = 1000000,
    alphabeta_smart_counted(Board, Player, Depth, Alpha, Beta, _Score, _Col, Nodes).

compare_all_algorithms(Board, Player, Depth) :-
    format('~n=== Comparaison des 4 algorithmes (profondeur ~w) ===~n', [Depth]),
    
    format('Minimax (basic)...~n'),
    get_time(S1),
    profile_minimax_nodes(Board, Player, Depth, N1),
    get_time(E1),
    T1 is (E1 - S1) * 1000,
    format('  ~w nœuds, ~3f ms~n', [N1, T1]),
    
    format('Alpha-Beta (basic)...~n'),
    get_time(S2),
    profile_alphabeta_nodes(Board, Player, Depth, N2),
    get_time(E2),
    T2 is (E2 - S2) * 1000,
    format('  ~w nœuds, ~3f ms~n', [N2, T2]),
    
    format('Minimax Smart...~n'),
    get_time(S3),
    profile_minimax_smart_nodes(Board, Player, Depth, N3),
    get_time(E3),
    T3 is (E3 - S3) * 1000,
    format('  ~w nœuds, ~3f ms~n', [N3, T3]),
    
    format('Alpha-Beta Smart...~n'),
    get_time(S4),
    profile_alphabeta_smart_nodes(Board, Player, Depth, N4),
    get_time(E4),
    T4 is (E4 - S4) * 1000,
    format('  ~w nœuds, ~3f ms~n', [N4, T4]).

