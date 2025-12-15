:- use_module(game).

:- use_module(simulator).

init_random :-
    writeln('=== Puissance 4 - IA Aleatoire ==='),
    writeln('IA: \U0001F534 (rouge) - Humain: \U0001F7E1 (jaune)'),
    nl,
    use_module(ia_random, []),
    play(ia_random, '\U0001F534').

init_random_plus :-
    writeln('=== Puissance 4 - IA Aleatoire Plus ==='),
    writeln('IA (gagne et bloque): \U0001F534 (rouge) - Humain: \U0001F7E1 (jaune)'),
    nl,
    use_module(ia_random_plus, []),
    play(ia_random_plus, '\U0001F534').

init_minimax :-
    writeln('=== Puissance 4 - IA Minimax ==='),
    writeln('IA Minimax: \U0001F534 (rouge) - Humain: \U0001F7E1 (jaune)'),
    nl,
    use_module(ia_minimax, []),
    play(ia_minimax, '\U0001F534').

init_hvh :-
    writeln('=== Puissance 4 - Humain vs Humain ==='),
    writeln('Joueur 1: \U0001F534 (rouge) - Joueur 2: \U0001F7E1 (jaune)'),
    nl,
    play_hvh('\U0001F534').

menu :-
    writeln(''),
    writeln('=============================================='),
    writeln('     PUISSANCE 4 - Choisissez votre action'),
    writeln('=============================================='),
    writeln(''),
    writeln('1. Jouer contre IA Aleatoire'),
    writeln('2. Jouer contre IA Aleatoire Plus'),
    writeln('3. Jouer contre IA Minimax'),
    writeln('4. Jouer Humain vs Humain'),
    writeln('5. SIMULER : Minimax vs Random Plus'),
    writeln('6. Lancer les tests'),
    writeln('7. Quitter'),
    writeln(''),
    write('Votre choix (1-7) : '),
    read_line_to_string(user_input, Line),
    normalize_space(atom(Atom), Line),
    (   atom_number(Atom, Choice),
        handle_choice(Choice)
    ;   writeln('Choix invalide!'),
        menu
    ).
handle_choice(1) :-
    nl,
    init_random,
    menu.
handle_choice(2) :-
    nl,
    init_random_plus,
    menu.
handle_choice(3) :-
    nl,
    init_minimax,
    menu.
handle_choice(4) :-
    nl,
    init_hvh,
    menu.
handle_choice(5) :-
    nl,
    simulate_ias_custom,
    menu.
handle_choice(6) :-
    writeln('Chargement des tests...'),
    consult('test_game.pl'),
    run_tests,
    menu.
handle_choice(7) :-
    writeln('Au revoir!').
handle_choice(_) :-
    writeln('Choix invalide!'),
    menu.

get_ia_choice(Prompt, IA_Module) :-
    writeln(''),
    format('--- Choix de l\'IA ~w ---~n', [Prompt]),
    writeln('1. ia_random'),
    writeln('2. ia_random_plus'),
    writeln('3. ia_minimax'),
    write('Votre choix (1-3) : '),
    read_line_to_string(user_input, Line),
    normalize_space(atom(Atom), Line),
    (   atom_number(Atom, Choice)
    ->  (   Choice = 1 -> IA_Module = ia_random
        ;   Choice = 2 -> IA_Module = ia_random_plus
        ;   Choice = 3 -> IA_Module = ia_minimax
        ;   writeln('Choix invalide.'),
            get_ia_choice(Prompt, IA_Module)
        )
    ;   writeln('Entrée invalide.'),
        get_ia_choice(Prompt, IA_Module)
    ).

simulate_ias_custom :-
    writeln(''),
    writeln('=============================================='),
    writeln('     SIMULATION D\'IAs'),
    writeln('=============================================='),
    
    % Sélection des deux IA
    get_ia_choice('A (Rouge)', IA_A),
    get_ia_choice('B (Jaune)', IA_B),
    
    % Si au moins une IA choisie est ia_minimax, charger le module et demander la profondeur pour chacune.
    % On stocke DAopt/DBopt (none si non applicable) et on laisse la simulation appliquer les mappings
    (   (IA_A = ia_minimax ; IA_B = ia_minimax)
    ->  use_module(ia_minimax, []),
        (   IA_A = ia_minimax
        ->  write('Profondeur du Minimax pour IA A (Rouge) (entier >= 1) : '),
            read_line_to_string(user_input, LineDA),
            normalize_space(atom(AtomDA), LineDA),
            (   atom_number(AtomDA, DA), integer(DA), DA >= 1
            ->  DAopt = DA
            ;   writeln('Profondeur invalide.'), simulate_ias_custom
            )
        ;   DAopt = none
        ),
        (   IA_B = ia_minimax
        ->  write('Profondeur du Minimax pour IA B (Jaune) (entier >= 1) : '),
            read_line_to_string(user_input, LineDB),
            normalize_space(atom(AtomDB), LineDB),
            (   atom_number(AtomDB, DB), integer(DB), DB >= 1
            ->  DBopt = DB
            ;   writeln('Profondeur invalide.'), simulate_ias_custom
            )
        ;   DBopt = none
        )
    ;   DAopt = none, DBopt = none
    ),
    
    write('Nombre de parties simulées (nombre pair pour faire commencer autant de fois chaque ia) : '),
    read_line_to_string(user_input, Line),
    normalize_space(atom(Atom), Line),
    (   atom_number(Atom, N),
        integer(N), N >= 2, N mod 2 =:= 0
    ->  writeln('Lancement de la simulation...'),
        % simulation avec les ias choisis (on transmet aussi DAopt/DBopt)
        ia_simulator:run_simulation(N, IA_A, IA_B, DAopt, DBopt)
    ;   writeln('Nombre invalide (doit être un entier pair >= 2).'),
        simulate_ias_custom
    ).

init :- menu.