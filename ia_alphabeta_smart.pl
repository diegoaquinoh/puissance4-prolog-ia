:- module(ia_alphabeta_smart, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

% IA Alpha-Beta "smart":
% - Détection tactique immédiate (win / block)
% - Recherche alpha-beta
% - Heuristique: contrôle du centre + scoring "windows of 4" (standard Connect 4)

ia(Board, Col, Player) :-
    % (Astuce) Avant de chercher, on regarde les tactiques "à 1 coup".
    % 1) Gagner si possible
    (   find_winning_move(Board, Player, WinCol)
    ->  Col = WinCol
    % 2) Bloquer si l'adversaire peut gagner
    ;   change_player(Player, Opp),
        find_winning_move(Board, Opp, BlockCol)
    ->  Col = BlockCol
    % 3) Sinon alphabeta
    ;   Depth = 6,
        Alpha = -1000000,
        Beta = 1000000,
        alphabeta(Board, Player, Depth, Alpha, Beta, _Score, BestCol),
        (   BestCol =:= -1
        ->  findall(C, legal_move(Board, C), [Col|_])
        ;   Col = BestCol
        )
    ),
    !.

find_winning_move(Board, Player, Col) :-
    % On teste chaque coup légal : si en jouant Col on gagne immédiatement, on le retourne.
    legal_move(Board, Col),
    play_move(Board, Col, NextBoard, Player),
    nth0(Col, NextBoard, ColumnAfter),
    length(ColumnAfter, Height),
    % Après play_move, le pion a été ajouté en haut de la colonne : sa ligne = Height - 1.
    Row is Height - 1,
    test_win(NextBoard, Player, Col, Row),
    !.

board_full(Board) :-
    match_nul(Board).

% Alpha-Beta pruning implementation
alphabeta(Board, _Player, Depth, _Alpha, _Beta, Score, -1) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    % Cas terminal: on évalue la position (heuristique ou win/loss).
    eval_board(Board, Score),
    !.

alphabeta(Board, Player, Depth, Alpha, Beta, BestScore, BestCol) :-
    Depth > 0,
    % On récupère tous les coups légaux (colonnes jouables)
    findall(C, legal_move(Board, C), Moves),
    % Important pour l'efficacité: jouer au centre d'abord -> plus de coupures alpha/beta.
    order_moves(Moves, OrderedMoves),
    (   Player = '\U0001F534'
    % Convention: Rouge maximise le score.
    ->  alphabeta_max(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol)
    % Jaune minimise le score.
    ;   alphabeta_min(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol)
    ).

% Maximizing player (Rouge)
alphabeta_max(_Board, _Player, _Depth, Alpha, _Beta, [], Alpha, -1).

alphabeta_max(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol) :-
    % On simule le coup Col
    play_move(Board, Col, NextBoard, Player),
    % On passe au joueur suivant
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    % Appel récursif : on évalue la position après le coup.
    alphabeta(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _),
    (   Score >= Beta
    ->  % Coupure Beta
        BestScore = Score,
        BestCol = Col
    ;   Score > Alpha
    ->  % Amélioration de Alpha
        alphabeta_max(Board, Player, Depth, Score, Beta, Rest, RestScore, RestCol),
        (   RestScore > Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   % Score <= Alpha, continue
        alphabeta_max(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol)
    ).

% Minimizing player (Jaune)
alphabeta_min(_Board, _Player, _Depth, _Alpha, Beta, [], Beta, -1).

alphabeta_min(Board, Player, Depth, Alpha, Beta, [Col|Rest], BestScore, BestCol) :-
    % On simule le coup Col
    play_move(Board, Col, NextBoard, Player),
    % On passe au joueur suivant
    change_player(Player, NextPlayer),
    Depth1 is Depth - 1,
    % Appel récursif : on évalue la position après le coup.
    alphabeta(NextBoard, NextPlayer, Depth1, Alpha, Beta, Score, _),
    (   Score =< Alpha
    ->  % Coupure Alpha
        BestScore = Score,
        BestCol = Col
    ;   Score < Beta
    ->  % Amélioration de Beta
        alphabeta_min(Board, Player, Depth, Alpha, Score, Rest, RestScore, RestCol),
        (   RestScore < Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   % Score >= Beta, continue
        alphabeta_min(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol)
    ).

% Order moves: try center columns first (better for pruning)
order_moves(Moves, Ordered) :-
    sort_by_center_distance(Moves, Ordered).

sort_by_center_distance(Moves, Sorted) :-
    maplist(add_distance, Moves, WithDist),
    keysort(WithDist, SortedWithDist),
    pairs_values(SortedWithDist, Sorted).

add_distance(Col, Dist-Col) :-
    % La colonne du centre est 3 (sur 0..6). Plus Dist est petit, plus c'est central.
    Dist is abs(Col - 3).

pairs_values([], []).
pairs_values([_-V|T], [V|VT]) :-
    pairs_values(T, VT).

% =========================
% Evaluation
% =========================

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

% Heuristique combinée:
% - contrôle du centre (matrice de poids) comme avant
% - + scoring "windows of 4" (horizontal/vertical/diagonales)
heuristic(Board, Score) :-
    center_control_score(Board, CenterScore),
    window4_score(Board, WindowScore),
    Score is CenterScore + WindowScore.

% ----- Heuristique 1 : contrôle du centre (déjà existante) -----
center_control_score(Board, Score) :-
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

% ----- Heuristique 2 : "Window of 4" scoring (nouvelle) -----

% window4_score(+Board, -Score)
% Score positif = avantage Rouge (\U0001F534), négatif = avantage Jaune (\U0001F7E1).
window4_score(Board, Score) :-
    findall(S,
            (   window4_cells(Board, Cells),
                score_window4(Cells, S)
            ),
            Scores),
    sum_list(Scores, Score).

% Enumerate every group of 4 cells ("window") on the 7x6 board.
window4_cells(Board, [A, B, C, D]) :-
    (   window4_horizontal(Board, [A, B, C, D])
    ;   window4_vertical(Board, [A, B, C, D])
    ;   window4_diag_up(Board, [A, B, C, D])
    ;   window4_diag_down(Board, [A, B, C, D])
    ).

window4_horizontal(Board, [A, B, C, D]) :-
    between(0, 5, Row),
    between(0, 3, Col),
    cell_or_empty(Board, Col, Row, A),
    Col1 is Col + 1, cell_or_empty(Board, Col1, Row, B),
    Col2 is Col + 2, cell_or_empty(Board, Col2, Row, C),
    Col3 is Col + 3, cell_or_empty(Board, Col3, Row, D).

window4_vertical(Board, [A, B, C, D]) :-
    between(0, 6, Col),
    between(0, 2, Row),
    cell_or_empty(Board, Col, Row, A),
    Row1 is Row + 1, cell_or_empty(Board, Col, Row1, B),
    Row2 is Row + 2, cell_or_empty(Board, Col, Row2, C),
    Row3 is Row + 3, cell_or_empty(Board, Col, Row3, D).

% Diagonale montante (\): (Col+i, Row+i)
window4_diag_up(Board, [A, B, C, D]) :-
    between(0, 3, Col),
    between(0, 2, Row),
    cell_or_empty(Board, Col, Row, A),
    Col1 is Col + 1, Row1 is Row + 1, cell_or_empty(Board, Col1, Row1, B),
    Col2 is Col + 2, Row2 is Row + 2, cell_or_empty(Board, Col2, Row2, C),
    Col3 is Col + 3, Row3 is Row + 3, cell_or_empty(Board, Col3, Row3, D).

% Diagonale descendante (/): (Col+i, Row-i)
window4_diag_down(Board, [A, B, C, D]) :-
    between(0, 3, Col),
    between(3, 5, Row),
    cell_or_empty(Board, Col, Row, A),
    Col1 is Col + 1, Row1 is Row - 1, cell_or_empty(Board, Col1, Row1, B),
    Col2 is Col + 2, Row2 is Row - 2, cell_or_empty(Board, Col2, Row2, C),
    Col3 is Col + 3, Row3 is Row - 3, cell_or_empty(Board, Col3, Row3, D).

% cell_or_empty(+Board, +Col, +Row, -Cell)
% Retourne 'empty' si la case est vide.
cell_or_empty(Board, Col, Row, Cell) :-
    (   get_cell(Board, Col, Row, Cell)
    ->  true
    ;   Cell = empty
    ).

% score_window4(+Cells, -Score)
% Standard Connect 4 "window" scoring.
% Weights (tunable):
% - myCount=4 -> huge
% - myCount=3, empty=1 -> +100
% - myCount=2, empty=2 -> +10
% - oppCount=3, empty=1 -> -120
% - oppCount=2, empty=2 -> -10
score_window4(Cells, Score) :-
    count_occurrences(Cells, '\U0001F534', MyCount),
    count_occurrences(Cells, '\U0001F7E1', OppCount),
    count_occurrences(Cells, empty, EmptyCount),
    (   MyCount > 0,
        OppCount > 0
    ->  % fenêtre "bloquée" (contient les 2 joueurs)
        Score = 0
    ;   MyCount =:= 4
    ->  Score = 100000
    ;   OppCount =:= 4
    ->  Score = -100000
    ;   MyCount =:= 3,
        EmptyCount =:= 1
    ->  Score = 100
    ;   MyCount =:= 2,
        EmptyCount =:= 2
    ->  Score = 10
    ;   OppCount =:= 3,
        EmptyCount =:= 1
    ->  Score = -120
    ;   OppCount =:= 2,
        EmptyCount =:= 2
    ->  Score = -10
    ;   Score = 0
    ).

count_occurrences([], _Elem, 0).
count_occurrences([X|Xs], Elem, Count) :-
    count_occurrences(Xs, Elem, Rest),
    (   X == Elem
    ->  Count is Rest + 1
    ;   Count = Rest
    ).
