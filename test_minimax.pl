% test_minimax.pl
:- begin_tests(minimax).

:- use_module(game).
:- use_module(ia_minimax).

% Helper to build a board from list of columns
board_from_cols(Cols, Cols).

% Test: IA must play the winning column when it can win in 1 move.
% We construct a board where Player = '\U0001F534' (Rouge) has a winning move at column 3.
% The board is given as list of 7 columns (each column is bottom-to-top list of cells).

test(winning_move_immediate) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    Board = [
        [\u0001F7E1],            % col 0: Yellow
        [\u0001F7E1, \u0001F534],% col 1: Yellow, Red
        [\u0001F534, \u0001F7E1, \u0001F534],% col 2
        [\u0001F534, \u0001F534, \u0001F7E1],% col 3 (after Red plays -> will be winning)
        [],
        [],
        []
    ],
    Player = '\U0001F534',
    ia_minimax:ia(Board, Col, Player),
    % Expect column 3 to be chosen as winning move
    assertion(Col =:= 3).

test(winning_move_immediate_bug) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    Board = [
        [\u0001F7E1, \u0001F7E1, \u0001F534, \u0001F7E1],            % col 0: Yellow
        [\u0001F7E1, \u0001F534, \u0001F7E1, \u0001F534],% col 1: Yellow, Red
        [\u0001F7E1, \u0001F7E1, \u0001F534, \u0001F534, \u0001F534, \u0001F7E1],% col 2
        [\u0001F534, \u0001F534, \u0001F534, \u0001F7E1, \u0001F534, \u0001F7E1],% col 3 (after Red plays -> will be winning)
        [\u0001F7E1, \u0001F534, \u0001F7E1, \u0001F534, \u0001F534, \u0001F534],
        [\u0001F7E1, \u0001F7E1, \u0001F7E1, \u0001F534, \u0001F7E1],
        [\u0001F534]
    ],
    Player = '\U0001F534',
    ia_minimax:ia(Board, Col, Player),
    % Expect column 3 to be chosen as winning move
    assertion(Col =:= 1).

test(winning_bug) :-
    % Example position (columns 0..6). Adjusted so that column 3 is the winning move for Rouge.
    % We'll build a scenario where Rouge has three in a row and playing column 3 completes four.
    % Columns: each inner list are cells from bottom (index 0) upwards.
    Board = [
        ['\u0001F7E1', '\u0001F7E1', '\u0001F7E1'],            % col 0: Yellow
        [],% col 1: Yellow, Red
        [],% col 2
        [],% col 3 (after Red plays -> will be winning)
        [],
        [],
        ['\u0001F534', '\u0001F534', '\u0001F534']
    ],
    Player = '\U0001F534',
    ia_minimax:ia(Board, Col, Player),
    % Expect column 3 to be chosen as winning move
    assertion(Col =:= 6).

:- end_tests(minimax).
