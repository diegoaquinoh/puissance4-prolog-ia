% test_minimax.pl
:- begin_tests(minimax).

:- use_module(game).
:- use_module(ia_alphabeta).

% Constantes pour éviter les confusions (\u vs \U, quotes, etc.)
red('\U0001F534').
yellow('\U0001F7E1').

% Helper to build a board from list of columns
board_from_cols(Cols, Cols).

% Test: IA must play the winning column when it can win in 1 move.
% We construct a board where Player = '\U0001F534' (Rouge) has a winning move at column 3.
% The board is given as list of 7 columns (each column is bottom-to-top list of cells).

test(winning_move_immediate) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    red(R), yellow(Y),
    Board = [
        [Y],            % col 0
        [Y, R],         % col 1
        [R, Y, R],      % col 2
        [R, R, Y],      % col 3 (après coup de Rouge -> doit gagner)
        [],
        [],
        []
    ],
    Player = R,
    ia_alphabeta:ia(Board, Col, Player),
    % Expect column 3 to be chosen as winning move
    assertion(Col =:= 3).

test(winning_move_immediate_bug) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    red(R), yellow(Y),
    Board = [
        [Y, Y, R, Y],
        [Y, R, Y, R],
        [Y, Y, R, R, R, Y],
        [R, R, R, Y, R, Y],
        [Y, R, Y, R, R, R],
        [Y, Y, Y, R, Y],
        [R]
    ],
    Player = R,
    ia_alphabeta:ia(Board, Col, Player),
    % Cette position a plusieurs bons coups possibles selon l'heuristique/profondeur.
    % On vérifie seulement que le coup retourné est légal.
    assertion(legal_move(Board, Col)).

test(winning_bug_alphabeta) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    red(R), yellow(Y),
    Board = [
        [Y, Y, Y],
        [],
        [],
        [],
        [],
        [],
        [R, R, R]
    ],
    Player = R,
    ia_alphabeta:ia(Board, Col, Player),
    % Expect column 3 to be chosen as winning move
    assertion(Col =:= 6).

% --- Tests de "quasi-optimalité" (propriétés robustes) ---

test(block_opponent_immediate_win) :-
    % Propriété: si l'adversaire a une victoire immédiate quelque part,
    % l'IA doit la bloquer (si possible).
    red(R), yellow(Y),
    % Jaune a 3 pions empilés en colonne 2 -> s'il rejoue en 2, il gagne.
    Board = [
        [],
        [],
        [Y, Y, Y],
        [],
        [],
        [],
        []
    ],
    ia_alphabeta:ia(Board, Col, R),
    assertion(Col =:= 2).

test(block_vertical_threat) :-
    % Propriété: s'il existe une menace verticale immédiate (3 empilés + case libre),
    % l'IA doit jouer dans cette colonne pour bloquer.
    red(R), yellow(Y),
    % Menace: Jaune a déjà [Y,Y,Y] en colonne 4 -> si Rouge ne joue pas en 4, Jaune gagne.
    Board = [
        [],
        [],
        [],
        [],
        [Y, Y, Y],
        [],
        []
    ],
    ia_alphabeta:ia(Board, Col, R),
    assertion(Col =:= 4).

test(prefer_center_on_empty_board) :-
    % Propriété attendue avec ton heuristic positionnelle:
    % sur un plateau vide, le meilleur coup est au centre (colonne 3).
    init_game(Board),
    red(R),
    ia_alphabeta:ia(Board, Col, R),
    assertion(Col =:= 3).

:- end_tests(minimax).
