:- module(ia_minimax, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

ia(Board, Col, Player) :-
    % 1) Vérifier s'il existe un coup gagnant immédiat pour le joueur
    (   find_winning_move(Board, Player, WinCol)
    ->  Col = WinCol
    % 2) Sinon, vérifier s'il faut bloquer un coup gagnant de l'adversaire
    ;   change_player(Player, Opponent),
        find_winning_move(Board, Opponent, BlockCol)
    ->  Col = BlockCol
    % 3) Sinon, utiliser minimax pour trouver le meilleur coup
    ;   Depth = 4,
        minimax(Board, Player, Depth, _Score, BestCol),
        (   BestCol =:= -1
        ->  findall(C, legal_move(Board, C), [Col|_])
        ;   Col = BestCol
        )
    ),
    !.

% Trouve un coup qui permet au joueur de gagner immédiatement
find_winning_move(Board, Player, Col) :-
    legal_move(Board, Col),
    play_move(Board, Col, NextBoard, Player),
    nth0(Col, NextBoard, ColumnAfter),
    length(ColumnAfter, Height),
    Row is Height - 1,
    test_win(NextBoard, Player, Col, Row),
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

% Heuristique améliorée : combine les poids positionnels avec l'analyse des fenêtres tactiques
heuristic(Board, Score) :-
    % Score positionnel (préférence pour le centre)
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
    PosScore is SX - SO,

    % Score tactique (fenêtres de 4 cases)
    windows_score(Board, WinScore),

    % Score total = position + tactique
    Score is PosScore + WinScore.

% Analyse toutes les fenêtres de 4 cases et calcule le score tactique
windows_score(Board, Total) :-
    findall(S, (
        % Fenêtres horizontales
        between(0, 5, Row),
        between(0, 3, ColStart),
        window_cells(Board, ColStart, Row, 1, 0, Cells),
        score_window(Cells, S)
    ), HScores),
    sum_list(HScores, HTotal),
    
    findall(S2, (
        % Fenêtres verticales
        between(0, 6, Col),
        between(0, 2, RowStart),
        window_cells(Board, Col, RowStart, 0, 1, Cells2),
        score_window(Cells2, S2)
    ), VScores),
    sum_list(VScores, VTotal),
    
    findall(S3, (
        % Diagonales descendantes (↘)
        between(0, 3, Col0),
        between(0, 2, Row0),
        window_cells(Board, Col0, Row0, 1, 1, Cells3),
        score_window(Cells3, S3)
    ), DScores1),
    sum_list(DScores1, DTotal1),
    
    findall(S4, (
        % Diagonales montantes (↗)
        between(0, 3, Col1),
        between(3, 5, Row1),
        window_cells(Board, Col1, Row1, 1, -1, Cells4),
        score_window(Cells4, S4)
    ), DScores2),
    sum_list(DScores2, DTotal2),
    
    Total is HTotal + VTotal + DTotal1 + DTotal2.

% Collecte 4 cellules consécutives à partir de (Col,Row) avec un pas (DX,DY)
window_cells(Board, Col, Row, DX, DY, Cells) :-
    C0 is Col, R0 is Row,
    cell_at(Board, C0, R0, Cell0),
    C1 is C0 + DX, R1 is R0 + DY,
    cell_at(Board, C1, R1, Cell1),
    C2 is C0 + 2*DX, R2 is R0 + 2*DY,
    cell_at(Board, C2, R2, Cell2),
    C3 is C0 + 3*DX, R3 is R0 + 3*DY,
    cell_at(Board, C3, R3, Cell3),
    Cells = [Cell0, Cell1, Cell2, Cell3].

% Récupère le contenu d'une cellule ou '.' si vide/hors-limites
cell_at(Board, Col, Row, Cell) :-
    (   get_cell(Board, Col, Row, C)
    ->  Cell = C
    ;   Cell = '.'
    ).

% Évalue une fenêtre de 4 cases selon les motifs tactiques
score_window(Cells, Score) :-
    count_elem('\U0001F534', Cells, RedCount),
    count_elem('\U0001F7E1', Cells, YellowCount),
    count_elem('.', Cells, EmptyCount),
    (   RedCount =:= 4 
    ->  Score = 100000  % 4 rouges alignés = victoire
    ;   YellowCount =:= 4 
    ->  Score = -100000  % 4 jaunes alignés = défaite
    ;   RedCount =:= 3, EmptyCount =:= 1 
    ->  Score = 5000  % 3 rouges + 1 vide = forte menace
    ;   YellowCount =:= 3, EmptyCount =:= 1 
    ->  Score = -8000  % 3 jaunes + 1 vide = bloquer!
    ;   RedCount =:= 2, EmptyCount =:= 2 
    ->  Score = 100  % 2 rouges + 2 vides = opportunité
    ;   YellowCount =:= 2, EmptyCount =:= 2 
    ->  Score = -150  % 2 jaunes + 2 vides = petite menace
    ;   Score = 0
    ).

% Compte les occurrences d'un élément dans une liste
count_elem(_, [], 0).
count_elem(E, [H|T], N) :-
    (   H == E
    ->  count_elem(E, T, N1), N is N1 + 1
    ;   count_elem(E, T, N)
    ).

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
