:- module(benchmark, [
    benchmark_ia_move/4,
    benchmark_ia_simulation/5,
    compare_ias_timed/4
]).

:- use_module(library(statistics)).
:- use_module(game).
:- use_module(simulator).

% ===== MESURE DE TEMPS PAR COUP =====

% Mesure le temps d'exécution d'un coup d'IA
% benchmark_ia_move(+IA_Module, +Board, +Player, -Stats)
% Stats = stats(Col, TimeMs, Inferences)
benchmark_ia_move(IA_Module, Board, Player, stats(Col, TimeMs, Inferences)) :-
    statistics(inferences, InfBefore),
    get_time(StartTime),
    
    % Appel de l'IA
    IA_Module:ia(Board, Col, Player),
    
    get_time(EndTime),
    statistics(inferences, InfAfter),
    
    TimeMs is (EndTime - StartTime) * 1000,
    Inferences is InfAfter - InfBefore.

% Affiche les statistiques d'un coup
print_move_stats(IA_Name, stats(Col, TimeMs, Inferences)) :-
    format('~w a joué colonne ~w en ~3f ms (~w inférences)~n', 
           [IA_Name, Col, TimeMs, Inferences]).

% ===== BENCHMARK SUR PLUSIEURS COUPS =====

% Teste une IA sur N coups depuis différentes positions
% benchmark_ia_on_positions(+IA_Module, +Positions, -AvgTimeMs, -AvgInferences)
benchmark_ia_on_positions(IA_Module, Positions, AvgTimeMs, AvgInferences) :-
    findall(stats(_, Time, Inf),
            (   member(pos(Board, Player), Positions),
                benchmark_ia_move(IA_Module, Board, Player, stats(_, Time, Inf))
            ),
            AllStats),
    length(AllStats, N),
    sum_stats(AllStats, TotalTime, TotalInf),
    AvgTimeMs is TotalTime / N,
    AvgInferences is TotalInf / N.

sum_stats([], 0, 0).
sum_stats([stats(_, Time, Inf)|Rest], TotalTime, TotalInf) :-
    sum_stats(Rest, RestTime, RestInf),
    TotalTime is RestTime + Time,
    TotalInf is RestInf + Inf.

% ===== SIMULATION AVEC TEMPS LIMITÉ =====

% Joue une partie avec timeout par coup (en secondes)
% Si l'IA dépasse le timeout, elle perd automatiquement
play_timed_game(IA1, IA2, TimeoutSec, Winner) :-
    init_game(Board),
    Player1 = '\U0001F534',
    Player2 = '\U0001F7E1',
    play_timed_loop(Board, Player1, IA1, IA2, TimeoutSec, Winner, Player1, Player2).

play_timed_loop(Board, Player, IA1, IA2, TimeoutSec, Winner, P1Symbol, P2Symbol) :-
    (   match_nul(Board)
    ->  Winner = draw
    ;   % Déterminer quelle IA joue
        (   Player = P1Symbol -> CurrentIA = IA1 ; CurrentIA = IA2 ),
        
        % Appel avec timeout
        catch(
            call_with_time_limit(TimeoutSec, CurrentIA:ia(Board, Move, Player)),
            time_limit_exceeded,
            (format('~w a dépassé le timeout de ~w sec!~n', [CurrentIA, TimeoutSec]),
             fail)
        ),
        
        % Jouer le coup
        play_move(Board, Move, NewBoard, Player),
        nth0(Move, NewBoard, Column),
        length(Column, H),
        Row is H - 1,
        
        % Vérifier victoire
        (   test_win(NewBoard, Player, Move, Row)
        ->  Winner = CurrentIA
        ;   change_player(Player, NextPlayer),
            play_timed_loop(NewBoard, NextPlayer, IA1, IA2, TimeoutSec, Winner, P1Symbol, P2Symbol)
        )
    ).

% Lance N parties avec timeout et retourne stats
% benchmark_ia_simulation(+N, +IA1, +IA2, +TimeoutSec, -Results)
% Results = results(Wins1, Wins2, Draws, Timeouts1, Timeouts2)
benchmark_ia_simulation(N, IA1, IA2, TimeoutSec, results(W1, W2, D, T1, T2)) :-
    N_half is N // 2,
    
    format('~n=== BENCHMARK : ~w vs ~w (timeout ~w sec/coup) ===~n', 
           [IA1, IA2, TimeoutSec]),
    
    % Première moitié : IA1 commence
    format('Phase 1/2 : ~w commence...~n', [IA1]),
    get_time(Start1),
    play_timed_games(N_half, IA1, IA2, TimeoutSec, R1),
    get_time(End1),
    Time1 is End1 - Start1,
    
    % Deuxième moitié : IA2 commence
    format('Phase 2/2 : ~w commence...~n', [IA2]),
    get_time(Start2),
    play_timed_games(N_half, IA2, IA1, TimeoutSec, R2),
    get_time(End2),
    Time2 is End2 - Start2,
    
    % Agréger résultats
    R1 = results(W1a, W2a, Da, T1a, T2a),
    R2 = results(W1b, W2b, Db, T1b, T2b),
    W1 is W1a + W2b,  % IA1 gagne quand elle commence ou quand IA2 commence
    W2 is W2a + W1b,
    D is Da + Db,
    T1 is T1a + T2b,
    T2 is T2a + T1b,
    
    TotalTime is Time1 + Time2,
    format('~nTemps total : ~2f sec~n', [TotalTime]),
    print_benchmark_results(N, IA1, IA2, results(W1, W2, D, T1, T2)).

play_timed_games(0, _, _, _, results(0, 0, 0, 0, 0)) :- !.
play_timed_games(N, IA1, IA2, TimeoutSec, results(W1, W2, D, T1, T2)) :-
    N > 0,
    (   catch(
            play_timed_game(IA1, IA2, TimeoutSec, Winner),
            error(time_limit_exceeded, _),
            Winner = timeout_error
        )
    ->  true
    ;   Winner = timeout_error
    ),
    
    N1 is N - 1,
    play_timed_games(N1, IA1, IA2, TimeoutSec, results(W1r, W2r, Dr, T1r, T2r)),
    
    (   Winner = IA1 -> W1 is W1r + 1, W2 = W2r, D = Dr, T1 = T1r, T2 = T2r
    ;   Winner = IA2 -> W1 = W1r, W2 is W2r + 1, D = Dr, T1 = T1r, T2 = T2r
    ;   Winner = draw -> W1 = W1r, W2 = W2r, D is Dr + 1, T1 = T1r, T2 = T2r
    ;   Winner = timeout_error -> W1 = W1r, W2 = W2r, D = Dr, T1 is T1r + 1, T2 = T2r
    ).

print_benchmark_results(N, IA1, IA2, results(W1, W2, D, T1, T2)) :-
    format('~n======================================================~n'),
    format('RÉSULTATS BENCHMARK : ~w vs ~w (~w parties)~n', [IA1, IA2, N]),
    format('======================================================~n'),
    P1 is (W1 / N) * 100,
    P2 is (W2 / N) * 100,
    PD is (D / N) * 100,
    format('Victoires ~w : ~w (~2f%%)~n', [IA1, W1, P1]),
    format('Victoires ~w : ~w (~2f%%)~n', [IA2, W2, P2]),
    format('Nuls : ~w (~2f%%)~n', [D, PD]),
    (   T1 > 0 -> format('Timeouts ~w : ~w~n', [IA1, T1]) ; true),
    (   T2 > 0 -> format('Timeouts ~w : ~w~n', [IA2, T2]) ; true),
    format('======================================================~n').

% ===== COMPARAISON DIRECTE =====

% Compare deux IAs sur positions de test standard
% compare_ias_timed(+IA1, +IA2, +TestPositions, -Comparison)
compare_ias_timed(IA1, IA2, TestPositions, comparison(IA1Avg, IA2Avg, SpeedupFactor)) :-
    format('~n=== Comparaison de vitesse : ~w vs ~w ===~n', [IA1, IA2]),
    
    format('Test de ~w...~n', [IA1]),
    benchmark_ia_on_positions(IA1, TestPositions, IA1Avg, IA1Inf),
    format('  Temps moyen : ~3f ms (~w inférences)~n', [IA1Avg, IA1Inf]),
    
    format('Test de ~w...~n', [IA2]),
    benchmark_ia_on_positions(IA2, TestPositions, IA2Avg, IA2Inf),
    format('  Temps moyen : ~3f ms (~w inférences)~n', [IA2Avg, IA2Inf]),
    
    SpeedupFactor is IA2Avg / IA1Avg,
    format('~nFacteur d\'accélération : ~2fx~n', [SpeedupFactor]),
    
    (   SpeedupFactor > 1.0
    ->  format('=> ~w est ~2fx plus rapide que ~w~n', [IA1, SpeedupFactor, IA2])
    ;   InverseSpeedup is 1 / SpeedupFactor,
        format('=> ~w est ~2fx plus rapide que ~w~n', [IA2, InverseSpeedup, IA1])
    ).

% ===== POSITIONS DE TEST STANDARD =====

% Génère des positions de test représentatives
generate_test_positions(Positions) :-
    Positions = [
        % Position vide (début de partie)
        pos([[],[],[],[],[],[],[]], '\U0001F534'),
        % Position milieu de partie
        pos([['\U0001F534'], ['\U0001F7E1'], ['\U0001F534'], [], ['\U0001F7E1'], [], []], '\U0001F534'),
        % Position complexe
        pos([['\U0001F534', '\U0001F7E1'], 
             ['\U0001F7E1', '\U0001F534'], 
             ['\U0001F534', '\U0001F7E1', '\U0001F534'], 
             ['\U0001F7E1'], 
             ['\U0001F534'], 
             [], 
             ['\U0001F7E1']], '\U0001F534')
    ].

% ===== COMMANDES RAPIDES =====

% Teste rapidement la vitesse de deux IAs
quick_speed_test(IA1, IA2) :-
    generate_test_positions(Positions),
    compare_ias_timed(IA1, IA2, Positions, _).

% Benchmark complet avec timeout
full_benchmark(IA1, IA2, NParties, TimeoutSec) :-
    benchmark_ia_simulation(NParties, IA1, IA2, TimeoutSec, _).
