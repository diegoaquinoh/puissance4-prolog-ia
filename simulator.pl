:- module(ia_simulator, [run_simulation/3]).

:- use_module(game, [
    init_game/1, 
    play_move/4, 
    test_win/4, 
    match_nul/1, 
    change_player/2
]).

:- use_module(ia_random, []).
:- use_module(ia_random_plus, []).
:- use_module(ia_minimax, []).
:- use_module(ia_minimax_smart, []).
:- use_module(ia_alphabeta, []).
:- use_module(ia_negamax, []).
:- use_module(ia_alphabeta_smart, []).


% --- Prédicats de Jeu pour la Simulation ---

% Le ModuleIA1 joue en premier (symbole ROUGE par convention dans votre code)
joue_partie(IA1, IA2, GagnantModule) :-
    init_game(BoardInit),
    J1Symbole = '\U0001F534', % Rouge
    J2Symbole = '\U0001F7E1', % Jaune
    
    % On simule une partie entre les deux ias
    simuler_boucle(BoardInit, J1Symbole, IA1, IA2, GagnantSymbole),

    % On associe le symbole gagnant à son module d'IA
    (   GagnantSymbole = J1Symbole -> GagnantModule = IA1
    ;   GagnantSymbole = J2Symbole -> GagnantModule = IA2
    ;   GagnantModule = nul
    ).

% Player est le symbole du joueur dont c'est le tour.
simuler_boucle(Board, Player, IA1, IA2, Gagnant) :-
    (   match_nul(Board)
    ->  Gagnant = nul
    ;   
        % Déterminer quelle ia doit jouer
        (   Player = '\U0001F534' -> CurrentIA = IA1
        ;   CurrentIA = IA2 
        ),

        % appel de l'ia pour choisir le coup
        CurrentIA:ia(Board, Move, Player),
        play_move(Board, Move, NewBoard, Player),
        
        % soit la case jouée a donné la win, soit on continue
        nth0(Move, NewBoard, ColumnAfter),
        length(ColumnAfter, H),
        Row is H - 1,

        (   test_win(NewBoard, Player, Move, Row)
        ->  Gagnant = Player % Victoire!
        ;   change_player(Player, NextPlayer),
            simuler_boucle(NewBoard, NextPlayer, IA1, IA2, Gagnant) % Continuer
        )
    ).

% --- Prédicats de Simulation et de Statistiques ---

% Lance N parties en alternant les joueurs et affiche les résultats.
run_simulation(N, IA1, IA2) :-
    % S'assurer que N est pair (en soit déjà fait lors de la sélection)
    ( N mod 2 =:= 0 -> N_demi is N // 2 ; N_demi is (N - 1) // 2 ),
    TotalParties is N_demi * 2,
    
    % Première moitié des parties avec IA1 qui commence (IA1=Rouge, IA2=Jaune)
    format('--- ~w vs ~w (Part 1 : ~w commence) ---~n', [IA1, IA2, IA1]),
    simuler_parties(N_demi, IA1, IA2, Res1),

    % IA2 commence N/2 fois (IA2=Rouge, IA1=Jaune)
    format('--- ~w vs ~w (Part 2 : ~w commence) ---~n', [IA2, IA1, IA2]),
    simuler_parties(N_demi, IA2, IA1, Res2),

    % On interprète les résultats
    aggr_resultats(Res1, Res2, VicIA1, VicIA2, Nuls),
    ecrire_stats(TotalParties, IA1, IA2, VicIA1, VicIA2, Nuls).

% IA1 joue avec le rouge (joueur 1) et IA2 avec le jaune (joueur 2) pour Count parties.
% Resultats est un terme res(VicIA1, VicIA2, Nuls)
simuler_parties(0, _, _, res(0, 0, 0)).
simuler_parties(N, IA1, IA2, res(VicIA1_Tot, VicIA2_Tot, Nuls_Tot)) :-
    N > 0,
    joue_partie(IA1, IA2, Gagnant),
    
    N_Moins_1 is N - 1,
    simuler_parties(N_Moins_1, IA1, IA2, res(VicIA1_Rest, VicIA2_Rest, Nuls_Rest)),
    
    % Mise à jour des totaux
    (
        Gagnant = IA1 ->
            VicIA1_Tot is VicIA1_Rest + 1,
            VicIA2_Tot is VicIA2_Rest,
            Nuls_Tot is Nuls_Rest
    ;
        Gagnant = IA2 ->
            VicIA1_Tot is VicIA1_Rest,
            VicIA2_Tot is VicIA2_Rest + 1,
            Nuls_Tot is Nuls_Rest
    ;
        % Gagnant = nul
        VicIA1_Tot is VicIA1_Rest,
        VicIA2_Tot is VicIA2_Rest,
        Nuls_Tot is Nuls_Rest + 1
    ).

% Res1 est (IA1 vs IA2), Res2 est (IA2 vs IA1)
aggr_resultats(res(V1a, V2a, Na), res(V1b, V2b, Nb), TotalV1, TotalV2, TotalNuls) :-
    % V1a = nb Victoires de IA1 (qui jouait ROUGE)
    % V2a = nb Victoires de IA2 (qui jouait JAUNE)
    % V1b = nb Victoires de IA2 (qui jouait ROUGE) 
    % V2b = nb Victoires de IA1 (qui jouait JAUNE) 

    % Total Victoires IA1 = (Victoires IA1 en ROUGE) + (Victoires IA1 en JAUNE)
    TotalV1 is V1a + V2b,
    
    % Total Victoires IA2 = (Victoires IA2 en JAUNE) + (Victoires IA2 en ROUGE)
    TotalV2 is V2a + V1b,

    TotalNuls is Na + Nb.


ecrire_stats(NombreParties, IA1, IA2, VicIA1, VicIA2, Nuls) :-
    format('~n======================================================~n'),
    format('--- RÉSULTATS FINAUX : ~w vs ~w sur ~w parties ---~n', [IA1, IA2, NombreParties]),
    format('======================================================~n'),
    
    % Calcul des pourcentages
    PourcIA1 is (VicIA1 / NombreParties) * 100,
    PourcIA2 is (VicIA2 / NombreParties) * 100,
    PourcNuls is (Nuls / NombreParties) * 100,

    format('Victoires ~w : ~w (~2f%%)~n', [IA1, VicIA1, PourcIA1]),
    format('Victoires ~w : ~w (~2f%%)~n', [IA2, VicIA2, PourcIA2]),
    format('Parties nulles : ~w (~2f%%)~n', [Nuls, PourcNuls]),
    format('======================================================~n').