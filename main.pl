:- use_module(game).

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

menu :-
    writeln(''),
    writeln('=============================================='),
    writeln('     PUISSANCE 4 - Choisissez votre IA'),
    writeln('=============================================='),
    writeln(''),
    writeln('1. IA Aleatoire       - Joue au hasard'),
    writeln('2. IA Aleatoire Plus  - Gagne et bloque'),
    writeln('3. IA Minimax         - Strategie avancee'),
    writeln('4. Quitter'),
    writeln(''),
    write('Votre choix (1-4) : '),
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
    writeln('Au revoir!').
handle_choice(_) :-
    writeln('Choix invalide!'),
    menu.

init :- menu.
