#!/bin/bash
# Test de comparaison ia_minimax vs ia_alphabeta

echo "=========================================="
echo "  Comparaison ia_minimax vs ia_alphabeta"
echo "=========================================="
echo ""
echo "Configuration:"
echo "  - ia_minimax:       Profondeur 3, heuristique simple"
echo "  - ia_alphabeta: Alpha-Beta + détection tactique"
echo ""
echo "Simulation de 50 parties (25 par couleur)..."
echo ""

swipl -s main.pl -g "ia_simulator:run_simulation(50, ia_minimax, ia_alphabeta), halt" 2>&1

echo ""
echo "Test terminé !"
echo ""
echo "Résultat attendu: ia_alphabeta devrait faire au moins aussi bien que ia_minimax (souvent beaucoup de nuls)"
