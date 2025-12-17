# 🚪 Nouvelle fonctionnalité : Quitter une partie en cours

## Comment quitter une partie ?

Pendant une partie, au lieu d'entrer un numéro de colonne (0-6), vous pouvez maintenant taper **`quit`** pour :
- ✅ Abandonner la partie en cours
- ✅ Retourner au menu principal
- ✅ Choisir un autre mode de jeu ou quitter le programme

## Exemple d'utilisation

```
=== Puissance 4 - IA Alpha-Beta ===
IA Alpha-Beta: 🔴 (rouge) - Humain: 🟡 (jaune)

┌─────────────────────────┐
│ 0   1   2   3   4   5   6│
│ .   .   .   .   .   .   .│
│ .   .   .   .   .   .   .│
│ .   .   .   .   .   .   .│
│ .   .   .   .   .   .   .│
│ .   .   .   .   .   .   .│
│ .   .   .   .   .   .   .│
└─────────────────────────┘

Tour de 🔴
IA joue colonne 3

Tour de 🟡
Colonne (0-6) ou "quit" pour quitter : quit
Retour au menu principal...

==============================================
     PUISSANCE 4 - Choisissez votre action
==============================================

1. Jouer contre IA Aleatoire
2. Jouer contre IA Aleatoire Plus
...
```

## Détails techniques

La fonctionnalité a été implémentée dans `game.pl` :

1. **Modification de `human_move/2`** : détecte la commande "quit" et lance une exception `quit_game`
2. **Modification de `play/2` et `play_hvh/0`** : capture l'exception avec `catch/3` et retourne proprement au menu

## Fichiers modifiés

- ✅ `game.pl` : ajout de la gestion de "quit"
  - `human_move/2` : détection de la commande quit
  - `play/2` : gestion de l'exception quit_game
  - `play_hvh/0` : gestion de l'exception quit_game

## Test

Pour tester la fonctionnalité :

```bash
# Lancer le jeu normalement
swipl -s main.pl -g init

# Choisir n'importe quel mode avec un humain (1-7 ou 8)
# Puis taper "quit" au lieu d'un numéro de colonne
```

Vous devriez voir "Retour au menu principal..." et revenir au menu.
