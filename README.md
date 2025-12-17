# puissance4-prolog-ia

H34
Diego AQUINO, COUPEY Zélie, KAHWATI Saria, VANNESTE Nathan, LEMSEFFER Mohammed, SAUCÉ Marc, Tora KAIJSER, Elijah SIMKIN, Erwann HEQUET

Jeu de Puissance 4 en Prolog avec différentes IAs.

## Architecture modulaire

Le projet est organisé de manière modulaire pour éviter la duplication de code :

- **`game.pl`** : Logique métier du jeu (plateau, affichage, règles, détection de victoire)
- **`ia_random.pl`** : IA qui joue au hasard
- **`ia_random_plus.pl`** : IA améliorée qui gagne quand c'est possible et bloque l'adversaire
- **`ia_minimax.pl`** : IA minimax avec évaluation heuristique simple (profondeur 4)
- **`ia_alphabeta.pl`** : IA alpha-beta avec évaluation heuristique simple (profondeur 6)
- **`ia_minimax_smart.pl`** : IA minimax avec évaluation heuristique avancée (profondeur 4)
- **`ia_alphabeta_smart.pl`** : IA alpha-beta avec évaluation heuristique avancée(profondeur 6)
- **`simulator.pl`** : Module de simulation pour comparer les IAs
- **`benchmark.pl`** : Outils de mesure de performance (temps, inférences)
- **`profiling.pl`** : Outils de comptage de nœuds explorés et pruning
- **`main.pl`** : Menu interactif pour choisir l'IA

Les fichiers test_* sont relatifs au testing et trace_game.pl pour du debugage utile pour analyser chaque coup choisi par une ia.

## Lancer une partie

### Méthode recommandée (avec menu de sélection)

```bash
swipl -s main.pl -g init
```

Vous pourrez ensuite choisir l'IA dans un menu interactif.

## Comment jouer

- L'IA joue avec 🔴 (rouge)
- L'humain joue avec 🟡 (jaune)
- Les cases vides sont représentées par ⚪ (blanc)
- Entrez un numéro de colonne entre 0 et 6 quand c'est votre tour
- Pour quitter une partie et retourner au menu : tapez `quit`
- Pour quitter complètement le programme : `Ctrl+C` puis `a`

## Personnalisation

Vous pouvez ajuster la profondeur des algos d'ias en modifiant la valeur de `Depth` dans le prédicat `ia/3`.

## Mesurer les performances

### Comparaison rapide de vitesse

```bash
swipl -s main.pl -g "use_module(benchmark), benchmark:quick_speed_test(ia_alphabeta, ia_minimax), halt"
```

### Compter les nœuds explorés (efficacité algorithmique)

```bash
swipl -s main.pl -g "use_module(profiling), init_game(B), profiling:compare_node_efficiency(B, '\U0001F534', 5), halt"
```

### Simulation avec timeout

```bash
swipl -s main.pl -g "use_module(benchmark), benchmark:full_benchmark(ia_alphabeta, ia_minimax, 20, 2.0), halt"
```

Pour plus de détails sur le benchmarking et les résultats, consultez [BENCHMARK_GUIDE.md](BENCHMARK_GUIDE.md).

## Tests
### Exécuter tous les tests en local

```bash
./run_tests.sh
```

### Exécuter les tests individuellement

**Tests du moteur de jeu :**
```bash
swipl -s main.pl -g "consult('test_game.pl'), run_tests, halt"
```

**Tests des algorithmes minimax/alpha-beta :**
```bash
swipl -s main.pl -g "consult('test_minimax.pl'), run_tests, halt"
```

### Intégration Continue (CI)

Les tests sont automatiquement exécutés sur GitHub Actions à chaque push/pull request.
Voir [.github/workflows/tests.yml](.github/workflows/tests.yml) pour la configuration.