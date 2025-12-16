:- use_module(game).
:- use_module(ia_minimax_smart, []).
:- use_module(ia_minimax, []).

trace_game(IA_Red, IA_Yellow) :-
    init_game(Board),
    trace_loop(Board, IA_Red, IA_Yellow, '\U0001F534', 1).

trace_loop(Board, _, _, _, _) :-
    match_nul(Board), !,
    writeln('==> DRAW').
    
trace_loop(Board, IA_Red, IA_Yellow, Player, MoveNum) :-
    (Player = '\U0001F534' -> IA = IA_Red ; IA = IA_Yellow),
    IA:ia(Board, Col, Player),
    format('Move ~w: ~w plays col ~w~n', [MoveNum, IA, Col]),
    play_move(Board, Col, NewBoard, Player),
    nth0(Col, NewBoard, Column),
    length(Column, H),
    Row is H - 1,
    (   test_win(NewBoard, Player, Col, Row)
    ->  format('==> ~w WINS after ~w moves!~n', [IA, MoveNum])
    ;   change_player(Player, Next),
        MoveNum1 is MoveNum + 1,
        trace_loop(NewBoard, IA_Red, IA_Yellow, Next, MoveNum1)
    ).

run :-
    writeln('=== GAME 1: ia_minimax_smart (Red) vs ia_minimax (Yellow) ==='),
    trace_game(ia_minimax_smart, ia_minimax),
    nl,
    writeln('=== GAME 2: ia_minimax (Red) vs ia_minimax_smart (Yellow) ==='),
    trace_game(ia_minimax, ia_minimax_smart).

