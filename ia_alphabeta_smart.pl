:- module(ia_alphabeta_smart, [ia/3]).

:- use_module(library(lists)).
:- use_module(game).

% ============================================================================
%                         IA ALPHABETA SMART
% ============================================================================
%
% An enhanced Alpha-Beta AI for Connect 4 with:
%   1. Alpha-Beta pruning for improved search efficiency
%   2. Immediate win/block detection (before alphabeta search)
%   3. Gravity-aware window scoring (playable vs floating threats)
%   4. Positional heuristic (center control preference)
%   5. Increased depth (6) thanks to alpha-beta efficiency
%
% The AI combines the smart heuristic of minimax_smart with alpha-beta pruning
% to search deeper while maintaining performance.
%
% ============================================================================

% ============================================================================
% HEURISTIC SCORING CONFIGURATION
% ============================================================================
%
% Final Score = positional_score * 10 + window_score
%
% - Positional: Rewards center control (max ~13 per piece, scaled by 10)
% - Window: Rewards threats and penalizes opponent threats (gravity-aware)
%
% The scaling ensures tactical situations (threats) can override positional
% preference, while center control breaks ties in equal tactical situations.

% Basic window scores (used only when all 4 cells are occupied)
window_score(4, 0, 0, 10000).    % 4 in a row = win
window_score(3, 0, 1, 50).       % 3 pieces + 1 empty
window_score(2, 0, 2, 5).        % 2 pieces + 2 empty
window_score(1, 0, 3, 1).        % 1 piece + 3 empty
window_score(0, 3, 1, -70).      % Opponent has 3 + 1 empty
window_score(0, 2, 2, -5).       % Opponent has 2 + 2 empty
window_score(0, 1, 3, -1).       % Opponent has 1 + 3 empty
window_score(_, _, _, 0).        % Mixed windows = blocked/neutral

% ============================================================================
% MAIN AI ENTRY POINT
% ============================================================================
%
% ia(+Board, -Col, +Player)
%   Board: Current game state (list of 7 columns)
%   Col: The chosen column to play (output)
%   Player: The player to move (🔴 or 🟡)
%
% Decision priority:
%   1. WIN: If we can win immediately, take it
%   2. BLOCK: If opponent wins next turn, block them
%   3. ALPHABETA: Search for best move with depth 6

ia(Board, Col, Player) :-
    (   winning_move(Board, Player, WinCol)
    ->  Col = WinCol
    ;   change_player(Player, Opponent),
        winning_move(Board, Opponent, BlockCol)
    ->  Col = BlockCol
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

% winning_move(+Board, +Player, -Col)
%   Checks if Player can win by playing in some column.
%   Returns the winning column if found.
winning_move(Board, Player, Col) :-
    legal_move(Board, Col),
    play_move(Board, Col, NewBoard, Player),
    nth0(Col, NewBoard, Column),
    length(Column, H),
    Row is H - 1,
    test_win(NewBoard, Player, Col, Row),
    !.

% ============================================================================
% ALPHA-BETA ALGORITHM
% ============================================================================

board_full(Board) :-
    match_nul(Board).

% Base case: Terminal node (depth 0, win, or draw)
% Returns score and -1 for column (no move at leaf)
alphabeta(Board, _Player, Depth, _Alpha, _Beta, Score, -1) :-
    (   Depth =:= 0
    ;   win_player(Board, '\U0001F534')
    ;   win_player(Board, '\U0001F7E1')
    ;   board_full(Board)
    ),
    eval_board(Board, Score),
    !.

% Recursive case: Explore moves with alpha-beta pruning
% Red (🔴) maximizes, Yellow (🟡) minimizes
alphabeta(Board, Player, Depth, Alpha, Beta, BestScore, BestCol) :-
    Depth > 0,
    Depth1 is Depth - 1,
    column_order(OrderedCols),
    (   Player = '\U0001F534'
    ->  alphabeta_max(Board, Player, Depth1, OrderedCols, Alpha, Beta, BestScore, BestCol)
    ;   alphabeta_min(Board, Player, Depth1, OrderedCols, Alpha, Beta, BestScore, BestCol)
    ).

% alphabeta_max: Maximizing player (Red)
alphabeta_max(_Board, _Player, _Depth, [], Alpha, _Beta, Alpha, -1) :- !.

alphabeta_max(Board, Player, Depth, [Col|RestCols], Alpha, Beta, BestScore, BestCol) :-
    (   legal_move(Board, Col)
    ->  play_move(Board, Col, NextBoard, Player),
        change_player(Player, NextPlayer),
        alphabeta(NextBoard, NextPlayer, Depth, Alpha, Beta, Score, _),
        (   Score >= Beta
        ->  % Beta cutoff - prune remaining branches
            BestScore = Score,
            BestCol = Col
        ;   Score > Alpha
        ->  % Update alpha and continue with new bound
            NewAlpha = Score,
            alphabeta_max(Board, Player, Depth, RestCols, NewAlpha, Beta, RestScore, RestCol),
            (   RestScore > Score
            ->  BestScore = RestScore, BestCol = RestCol
            ;   BestScore = Score, BestCol = Col
            )
        ;   % Score <= Alpha, continue with same alpha
            alphabeta_max(Board, Player, Depth, RestCols, Alpha, Beta, BestScore, BestCol)
        )
    ;   % Column not legal, try next
        alphabeta_max(Board, Player, Depth, RestCols, Alpha, Beta, BestScore, BestCol)
    ).

% alphabeta_min: Minimizing player (Yellow)
alphabeta_min(_Board, _Player, _Depth, [], _Alpha, Beta, Beta, -1) :- !.

alphabeta_min(Board, Player, Depth, [Col|RestCols], Alpha, Beta, BestScore, BestCol) :-
    (   legal_move(Board, Col)
    ->  play_move(Board, Col, NextBoard, Player),
        change_player(Player, NextPlayer),
        alphabeta(NextBoard, NextPlayer, Depth, Alpha, Beta, Score, _),
        (   Score =< Alpha
        ->  % Alpha cutoff - prune remaining branches
            BestScore = Score,
            BestCol = Col
        ;   Score < Beta
        ->  % Update beta and continue with new bound
            NewBeta = Score,
            alphabeta_min(Board, Player, Depth, RestCols, Alpha, NewBeta, RestScore, RestCol),
            (   RestScore < Score
            ->  BestScore = RestScore, BestCol = RestCol
            ;   BestScore = Score, BestCol = Col
            )
        ;   % Score >= Beta, continue with same beta
            alphabeta_min(Board, Player, Depth, RestCols, Alpha, Beta, BestScore, BestCol)
        )
    ;   % Column not legal, try next
        alphabeta_min(Board, Player, Depth, RestCols, Alpha, Beta, BestScore, BestCol)
    ).

% Column evaluation order (Optimized: Center first for better pruning)
column_order([3, 2, 4, 1, 5, 0, 6]).

% eval_board(+Board, -Score)
%   Returns terminal score for wins, or heuristic for non-terminal
eval_board(Board, Score) :-
    (   win_player(Board, '\U0001F534')
    ->  Score = 100000
    ;   win_player(Board, '\U0001F7E1')
    ->  Score = -100000
    ;   heuristic(Board, Score)
    ).

% win_player(+Board, +Player)
%   True if Player has won the game
win_player(Board, Player) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player),
    test_win(Board, Player, Col, Row),
    !.

% ============================================================================
% COMBINED HEURISTIC
% ============================================================================
%
% Score = positional_score * 10 + window_score
%
% Positional handles center preference, window handles tactical evaluation.

heuristic(Board, Score) :-
    positional_heuristic(Board, PosScore),
    window_heuristic(Board, WinScore),
    Score is PosScore * 10 + WinScore.

% ============================================================================
% POSITIONAL HEURISTIC
% ============================================================================
%
% Evaluates board based on piece positions using a weight matrix.
% Center positions are worth more (more winning opportunities).
%
% Score = sum(Red piece weights) - sum(Yellow piece weights)

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

% any_cell(+Board, +Player, -Col, -Row)
%   Finds all cells occupied by Player
any_cell(Board, Player, Col, Row) :-
    nth0(Col, Board, Column),
    nth0(Row, Column, Player).

% Cell weight matrix (center-heavy)
%   Row 0 = bottom, Row 5 = top
%   Col 3 = center column (highest weights)
cell_weight_matrix([
    [1, 2, 4, 7, 4, 2, 1],    % Row 0 (bottom)
    [2, 4, 6, 10, 6, 4, 2],   % Row 1
    [3, 5, 8, 13, 8, 5, 3],   % Row 2
    [3, 5, 8, 13, 8, 5, 3],   % Row 3
    [2, 4, 6, 10, 6, 4, 2],   % Row 4
    [1, 2, 4, 7, 4, 2, 1]     % Row 5 (top)
]).

cell_weight(Row, Col, W) :-
    cell_weight_matrix(Matrix),
    nth0(Row, Matrix, Line),
    nth0(Col, Line, W).

% ============================================================================
% WINDOW HEURISTIC (Gravity-Aware)
% ============================================================================
%
% Scans all possible 4-cell windows (horizontal, vertical, diagonal) and
% scores them based on PLAYABILITY of empty cells.
%
% Threat Classification:
%   - IMMEDIATE: Empty cell is at column height (playable now)
%   - NEAR: Empty cell is 1-2 rows above column height
%   - FLOATING: Empty cell is 3+ rows above column height
%
% Immediate threats score much higher because they can be completed/blocked
% on the next move. Floating threats are nearly worthless.

window_heuristic(Board, Score) :-
    get_column_heights(Board, Heights),
    score_all_windows(Board, Heights, Score).

% get_column_heights(+Board, -Heights)
%   Returns list of 7 heights (number of pieces in each column)
get_column_heights(Board, Heights) :-
    maplist(length, Board, Heights).

% score_all_windows(+Board, +Heights, -Score)
%   Sums scores of all 69 possible windows
score_all_windows(Board, Heights, Score) :-
    findall(S,
            (   window_with_positions(Board, Window, Positions),
                score_window_gravity(Window, Positions, Heights, S)
            ),
            Scores),
    sum_list(Scores, Score).

% ============================================================================
% WINDOW GENERATION
% ============================================================================
%
% Generates all windows of 4 consecutive cells with their positions.
% A window is a list of 4 cells; positions track (Col, Row) for each.

% Horizontal windows (24 total: 6 rows × 4 starting columns)
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

% Vertical windows (21 total: 7 columns × 3 starting rows)
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

% Diagonal ↗ windows (12 total: 4 start cols × 3 start rows)
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

% Diagonal ↘ windows (12 total: 4 start cols × 3 start rows)
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

% get_cell_or_empty(+Board, +Col, +Row, -Cell)
%   Returns cell content or 'empty' atom for unoccupied cells
get_cell_or_empty(Board, Col, Row, Cell) :-
    nth0(Col, Board, Column),
    (   nth0(Row, Column, C)
    ->  Cell = C
    ;   Cell = empty
    ).

% ============================================================================
% GRAVITY-AWARE SCORING
% ============================================================================

% score_window_gravity(+Window, +Positions, +Heights, -Score)
%   Scores a window based on piece counts and empty cell playability
score_window_gravity(Window, Positions, Heights, Score) :-
    count_in_window(Window, '\U0001F534', MyCount),
    count_in_window(Window, '\U0001F7E1', OppCount),
    EmptyCount is 4 - MyCount - OppCount,
    (   EmptyCount =:= 0
    ->  % Full window - use basic scoring
        window_score(MyCount, OppCount, EmptyCount, Score)
    ;   % Has empty cells - classify threat by playability
        find_empty_positions(Window, Positions, EmptyPositions),
        classify_threat(EmptyPositions, Heights, ThreatType),
        window_score_gravity(MyCount, OppCount, ThreatType, Score)
    ).

% find_empty_positions(+Window, +Positions, -EmptyPositions)
%   Extracts positions of empty cells from the window
find_empty_positions([], [], []).
find_empty_positions([Cell|Cells], [Pos|Positions], EmptyPos) :-
    (   Cell == empty
    ->  EmptyPos = [Pos|RestEmpty]
    ;   EmptyPos = RestEmpty
    ),
    find_empty_positions(Cells, Positions, RestEmpty).

% classify_threat(+EmptyPositions, +Heights, -ThreatType)
%   Classifies threat as immediate/near/floating based on playability
classify_threat(EmptyPositions, Heights, ThreatType) :-
    (   any_playable_now(EmptyPositions, Heights)
    ->  ThreatType = immediate
    ;   any_near_playable(EmptyPositions, Heights)
    ->  ThreatType = near
    ;   ThreatType = floating
    ).

% any_playable_now(+EmptyPositions, +Heights)
%   True if any empty cell is at its column's current height (playable now)
any_playable_now(EmptyPositions, Heights) :-
    member(pos(Col, Row), EmptyPositions),
    nth0(Col, Heights, Height),
    Row =:= Height,
    !.

% any_near_playable(+EmptyPositions, +Heights)
%   True if any empty cell is 1-2 rows above playable height
any_near_playable(EmptyPositions, Heights) :-
    member(pos(Col, Row), EmptyPositions),
    nth0(Col, Heights, Height),
    Diff is Row - Height,
    Diff >= 1, Diff =< 2,
    !.

% ============================================================================
% GRAVITY-AWARE SCORE VALUES
% ============================================================================
%
% Immediate threats: Full tactical value (can be completed/blocked now)
% Near threats: Reduced value (will become playable soon)
% Floating threats: Minimal value (far from playable)

% Win detection
window_score_gravity(4, 0, _, 10000) :- !.

% Our 3-in-a-row threats
window_score_gravity(3, 0, immediate, 100) :- !.   % Can complete NOW
window_score_gravity(3, 0, near, 30) :- !.         % Completable soon
window_score_gravity(3, 0, floating, 5) :- !.      % Far from completable

% Our 2-in-a-row setups
window_score_gravity(2, 0, immediate, 20) :- !.
window_score_gravity(2, 0, near, 8) :- !.
window_score_gravity(2, 0, floating, 2) :- !.

% Opponent's 3-in-a-row threats (negative = bad for us)
window_score_gravity(0, 3, immediate, -120) :- !.  % MUST BLOCK!
window_score_gravity(0, 3, near, -40) :- !.        % Prepare defense
window_score_gravity(0, 3, floating, -5) :- !.     % Low priority

% Opponent's 2-in-a-row setups
window_score_gravity(0, 2, immediate, -20) :- !.
window_score_gravity(0, 2, near, -8) :- !.
window_score_gravity(0, 2, floating, -2) :- !.

% Mixed or single pieces (blocked or weak)
window_score_gravity(_, _, _, 0).

% count_in_window(+Window, +Player, -Count)
%   Counts how many of Player's pieces are in the window
count_in_window(Window, Player, Count) :-
    include(==(Player), Window, Matches),
    length(Matches, Count).
