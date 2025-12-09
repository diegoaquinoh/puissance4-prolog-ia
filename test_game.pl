:- begin_tests(game).
:- use_module(game).

% Test de l initialisation du plateau

 test(init_game) :-
    init_game(Board),
    length(Board, 7),
    maplist(=([]), Board).

% Test du changement de joueur
 test(change_player) :-
    change_player('\U0001F534', P), P = '\U0001F7E1',
    change_player('\U0001F7E1', P2), P2 = '\U0001F534'.

% Test d un coup joué
 test(play_move) :-
    init_game(Board),
    play_move(Board, 0, NewBoard, '\U0001F534'),
    nth0(0, NewBoard, Col), Col = ['\U0001F534'].

% Test de la détection de match nul
 test(match_nul_false) :-
    init_game(Board),
    \+ match_nul(Board).

 test(match_nul_true) :-
    Board = [['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534'],
             ['\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534','\U0001F534']],
    match_nul(Board).

% Test de la détection de victoire horizontale
 test(win_horizontal) :-
    Board = [
        ['\U0001F534','\U0001F534','\U0001F534','\U0001F534'],[],[],[],[],[],[]
    ],
    test_win(Board, '\U0001F534', 0, 3).

% Test de la détection de victoire verticale
 test(win_vertical) :-
    Board = [
        ['\U0001F534'],['\U0001F534'],['\U0001F534'],['\U0001F534'],[],[],[]
    ],
    test_win(Board, '\U0001F534', 3, 0).

:- end_tests(game).
