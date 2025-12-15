:- module(ia_alphabeta, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

% IA Alpha-Beta : ajout de la détection tactique immédiate (win/block)
% + recherche alpha-beta.
ia(Board, Col, Player) :-
    % (Astuce pratique) Avant de chercher, on regarde les tactiques "à 1 coup".
    % Ça évite de lancer l'alpha-beta pour rien quand on peut gagner/bloquer tout de suite.
    % 1) Gagner si possible
    (   find_winning_move(Board, Player, WinCol)
    ->  Col = WinCol
    % 2) Bloquer si l'adversaire peut gagner
    ;   change_player(Player, Opp),
        find_winning_move(Board, Opp, BlockCol)
    ->  Col = BlockCol
    % 3) Sinon alphabeta
    ;   Depth = 6,
        % Alpha = meilleure valeur garantie pour le joueur courant (borne inférieure)
        Alpha = -1000000,
        % Beta = pire valeur acceptable pour le joueur courant (borne supérieure)
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
    % Convention ici: Rouge maximise le score (il cherche un grand Score).
    ->  alphabeta_max(Board, Player, Depth, Alpha, Beta, OrderedMoves, BestScore, BestCol)
    % Jaune minimise le score (il cherche un petit Score).
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
    ->  % Coupure Beta : le joueur max a trouvé un coup "trop bon".
        % Le joueur min au-dessus ne laissera jamais arriver cette position.
        BestScore = Score,
        BestCol = Col
    ;   Score > Alpha
    ->  % Amélioration de Alpha : on a trouvé un meilleur coup pour max.
        alphabeta_max(Board, Player, Depth, Score, Beta, Rest, RestScore, RestCol),
        (   RestScore > Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   % Score <= Alpha, continue
        % Pas meilleur que ce qu'on a déjà : on teste les autres coups.
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
    ->  % Coupure Alpha : le joueur min a trouvé un coup "trop mauvais" pour max.
        % Le joueur max au-dessus choisira un autre coup et n'ira pas dans cette branche.
        BestScore = Score,
        BestCol = Col
    ;   Score < Beta
    ->  % Amélioration de Beta : on a trouvé un meilleur coup pour min (plus petit score).
        alphabeta_min(Board, Player, Depth, Alpha, Score, Rest, RestScore, RestCol),
        (   RestScore < Score
        ->  BestScore = RestScore, BestCol = RestCol
        ;   BestScore = Score, BestCol = Col
        )
    ;   % Score >= Beta, continue
        % Pas meilleur que ce qu'on a déjà : on teste les autres coups.
        alphabeta_min(Board, Player, Depth, Alpha, Beta, Rest, BestScore, BestCol)
    ).

% Order moves: try center columns first (better for pruning)
order_moves(Moves, Ordered) :-
    % L'idée : plus on trouve tôt de bons coups, plus alpha/beta se resserrent,
    % donc plus on coupe de branches.
    sort_by_center_distance(Moves, Ordered).

sort_by_center_distance(Moves, Sorted) :-
    % On transforme chaque Col en paire "DistanceAuCentre-Col",
    % puis keysort trie par clé (la distance).
    maplist(add_distance, Moves, WithDist),
    keysort(WithDist, SortedWithDist),
    pairs_values(SortedWithDist, Sorted).

add_distance(Col, Dist-Col) :-
    % La colonne du centre est 3 (sur 0..6). Plus Dist est petit, plus c'est central.
    Dist is abs(Col - 3).

pairs_values([], []).
pairs_values([_-V|T], [V|VT]) :-
    pairs_values(T, VT).

eval_board(Board, Score) :-
    (   win_player(Board, '\U0001F534')
    ->  Score = 100000
    ;   win_player(Board, '\U0001F7E1')
    ->  Score = -100000
    ;   heuristic(Board, Score)
    ).

win_player(Board, Player) :-
    % On cherche un pion du joueur, puis on demande à test_win/4 s'il y a 4 alignés.
    % Dès qu'on en trouve un gagnant, on coupe (!) pour ne pas backtracker.
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

% Heuristique positionnelle (centre)
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
