# puissance4-prolog-ia

Jeu de Puissance 4 en Prolog avec différentes IAs.

## Architecture modulaire

Le projet est organisé de manière modulaire pour éviter la duplication de code :

- **`game.pl`** : Logique métier du jeu (plateau, affichage, règles, détection de victoire)
- **`ia_random.pl`** : IA qui joue au hasard
- **`ia_random_plus.pl`** : IA améliorée qui gagne quand c'est possible et bloque l'adversaire
- **`ia_minimax.pl`** : IA minimax avec évaluation heuristique (profondeur 4)
- **`main.pl`** : Menu interactif pour choisir l'IA

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
- Pour quitter : `Ctrl+C` puis `a`

## Personnalisation

Vous pouvez ajuster la profondeur du minimax dans `ia_minimax.pl` en modifiant la valeur de `Depth` dans le prédicat `ia/3`.