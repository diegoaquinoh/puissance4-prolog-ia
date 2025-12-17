% Test rapide de la fonctionnalité quit
:- use_module(game).

test_quit :-
    writeln('Test de la fonctionnalité quit...'),
    writeln('Si vous tapez "quit", vous devriez revenir au test.'),
    nl,
    catch(
        (   init_game(Board),
            display_board(Board),
            writeln('Tapez "quit" pour tester la fonctionnalité'),
            human_move(Board, _Col),
            writeln('Vous avez joué un coup normal.')
        ),
        quit_game,
        writeln('✓ La commande quit fonctionne ! Retour au menu.')
    ),
    nl,
    writeln('Test terminé.').

% Pour lancer : swipl -s test_quit.pl -g "test_quit, halt"
