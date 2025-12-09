# puissance4-prolog-ia

Deux variantes de Puissance 4 en Prolog :

- `aleatoire.pl` : IA qui joue au hasard (pions `x`) contre un humain (`o`).
- `aleatoirePlus.pl` : IA qui joue au hasard (pions `x`) contre un humain (`o`). Si un coup gagnant existe, elle le joue. Si l'adversaire a un coup gagnant, elle le bloque.
- `minmax.pl` : IA minimax (profondeur 4 par défaut) pour `x`.

## Lancer une partie

Depuis la racine du projet :

```bash
swipl -s aleatoire.pl -g init
# ou
swipl -s aleatoirePlus.pl -g init
# ou
swipl -s minmax.pl -g init
```

Ensuite, entrez un numéro de colonne entre 0 et 6 quand c'est au tour du joueur humain. Vous pouvez ajuster la profondeur du minimax dans `ia/3` si besoin.