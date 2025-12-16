#!/bin/bash

# Script de benchmark complet : Minimax vs Alpha-Beta (basic et smart)
# Compare les 4 algorithmes à différentes profondeurs (1-8)
# et génère un fichier CSV avec les résultats

# Configuration
OUTPUT_CSV="benchmark_results.csv"
DEPTHS=(1 2 3 4 5 6 7 8)
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
TIMEOUT_SECONDS=300  # 5 minutes max par profondeur

# Couleurs
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}======================================"
echo "  Benchmark: 4 Algorithmes"
echo "  minimax, alphabeta, minimax_smart, alphabeta_smart"
echo "  Profondeurs: ${DEPTHS[@]}"
echo "======================================${NC}"
echo ""

# Créer le header du CSV
echo "algorithme,profondeur,noeuds_explores,temps_ms" > "$OUTPUT_CSV"

# Fonction pour exécuter le profiling et parser les résultats
run_profiling() {
    local depth=$1
    local output_file="temp_profiling_${depth}.txt"
    
    echo -e "${BLUE}[Profondeur $depth] Exécution du profiling (4 algorithmes)...${NC}"
    
    # Lancer le profiling avec timeout et capturer la sortie
    # Sur macOS, utiliser gtimeout si disponible, sinon pas de timeout
    if command -v gtimeout &> /dev/null; then
        gtimeout $TIMEOUT_SECONDS swipl -s main.pl -g "use_module(profiling), init_game(B), profiling:compare_all_algorithms(B, '🔴', $depth), halt" > "$output_file" 2>&1
    else
        swipl -s main.pl -g "use_module(profiling), init_game(B), profiling:compare_all_algorithms(B, '🔴', $depth), halt" > "$output_file" 2>&1
    fi
    
    local exit_code=$?
    
    if [ $exit_code -eq 124 ]; then
        echo -e "${RED}⏱  Timeout après ${TIMEOUT_SECONDS}s à profondeur $depth${NC}"
        rm -f "$output_file"
        return 1
    elif [ $exit_code -ne 0 ]; then
        echo -e "${RED}Erreur lors de l'exécution à profondeur $depth${NC}"
        rm -f "$output_file"
        return 1
    fi
    
    # Parser les résultats pour les 4 algorithmes
    # Format attendu: "  <nodes> nœuds, <time> ms"
    
    # Minimax basic
    minimax_line=$(grep -A1 "Minimax (basic)" "$output_file" | tail -1)
    minimax_nodes=$(echo "$minimax_line" | awk '{print $1}')
    minimax_time=$(echo "$minimax_line" | sed -E 's/.* ([0-9.]+) ms.*/\1/')
    
    # Alpha-Beta basic
    alphabeta_line=$(grep -A1 "Alpha-Beta (basic)" "$output_file" | tail -1)
    alphabeta_nodes=$(echo "$alphabeta_line" | awk '{print $1}')
    alphabeta_time=$(echo "$alphabeta_line" | sed -E 's/.* ([0-9.]+) ms.*/\1/')
    
    # Minimax Smart
    minimax_smart_line=$(grep -A1 "Minimax Smart" "$output_file" | tail -1)
    minimax_smart_nodes=$(echo "$minimax_smart_line" | awk '{print $1}')
    minimax_smart_time=$(echo "$minimax_smart_line" | sed -E 's/.* ([0-9.]+) ms.*/\1/')
    
    # Alpha-Beta Smart
    alphabeta_smart_line=$(grep -A1 "Alpha-Beta Smart" "$output_file" | tail -1)
    alphabeta_smart_nodes=$(echo "$alphabeta_smart_line" | awk '{print $1}')
    alphabeta_smart_time=$(echo "$alphabeta_smart_line" | sed -E 's/.* ([0-9.]+) ms.*/\1/')
    
    # Écrire dans le CSV
    echo "minimax,$depth,$minimax_nodes,$minimax_time" >> "$OUTPUT_CSV"
    echo "alphabeta,$depth,$alphabeta_nodes,$alphabeta_time" >> "$OUTPUT_CSV"
    echo "minimax_smart,$depth,$minimax_smart_nodes,$minimax_smart_time" >> "$OUTPUT_CSV"
    echo "alphabeta_smart,$depth,$alphabeta_smart_nodes,$alphabeta_smart_time" >> "$OUTPUT_CSV"
    
    # Afficher un résumé
    echo -e "${GREEN}  Minimax       : $minimax_nodes nœuds, ${minimax_time} ms${NC}"
    echo -e "${GREEN}  Alpha-Beta    : $alphabeta_nodes nœuds, ${alphabeta_time} ms${NC}"
    echo -e "${GREEN}  Minimax Smart : $minimax_smart_nodes nœuds, ${minimax_smart_time} ms${NC}"
    echo -e "${GREEN}  Alpha-Beta Smt: $alphabeta_smart_nodes nœuds, ${alphabeta_smart_time} ms${NC}"
    
    # Calculer quelques stats
    if [ "$minimax_nodes" != "" ] && [ "$alphabeta_nodes" != "" ]; then
        reduction=$(echo "scale=2; $minimax_nodes / $alphabeta_nodes" | bc 2>/dev/null)
        if [ "$reduction" != "" ]; then
            echo -e "${YELLOW}  → Pruning alpha-beta: ${reduction}x réduction${NC}"
        fi
    fi
    echo ""
    
    # Nettoyer
    rm -f "$output_file"
}

# Boucle sur toutes les profondeurs
for depth in "${DEPTHS[@]}"; do
    echo -e "${BLUE}═══════════════════════════════════════${NC}"
    echo -e "${BLUE}  Profondeur $depth${NC}"
    echo -e "${BLUE}═══════════════════════════════════════${NC}"
    
    # Vérifier si la profondeur est trop élevée (timeout possible)
    if [ "$depth" -ge 7 ]; then
        echo -e "${YELLOW}⚠️  Attention: Profondeur $depth peut être très lente pour minimax${NC}"
        echo -e "${YELLOW}   Estimation: plusieurs minutes (timeout: ${TIMEOUT_SECONDS}s)...${NC}"
        echo ""
    fi
    
    # Exécuter le profiling
    if run_profiling "$depth"; then
        echo -e "${GREEN}✓ Profondeur $depth terminée${NC}"
    else
        echo -e "${RED}✗ Profondeur $depth échouée (timeout ou erreur)${NC}"
        # Ajouter une ligne avec des valeurs manquantes pour les 4 algorithmes
        echo "minimax,$depth,TIMEOUT,TIMEOUT" >> "$OUTPUT_CSV"
        echo "alphabeta,$depth,TIMEOUT,TIMEOUT" >> "$OUTPUT_CSV"
        echo "minimax_smart,$depth,TIMEOUT,TIMEOUT" >> "$OUTPUT_CSV"
        echo "alphabeta_smart,$depth,TIMEOUT,TIMEOUT" >> "$OUTPUT_CSV"
    fi
    echo ""
done

echo -e "${BLUE}======================================"
echo "  Benchmark terminé !"
echo "======================================${NC}"
echo ""
echo -e "Résultats sauvegardés dans: ${GREEN}$OUTPUT_CSV${NC}"
echo ""

# Générer un résumé visuel
echo -e "${BLUE}Résumé des résultats (premières lignes):${NC}"
echo ""
head -20 "$OUTPUT_CSV" | column -t -s','

# Créer une copie avec timestamp
cp "$OUTPUT_CSV" "benchmark_results_${TIMESTAMP}.csv"
echo ""
echo -e "Copie archivée: ${GREEN}benchmark_results_${TIMESTAMP}.csv${NC}"
echo ""

echo -e "${GREEN}✓ Benchmark complet !${NC}"
echo ""
echo "Pour visualiser les données:"
echo "  - Ouvrir $OUTPUT_CSV dans Excel/LibreOffice"
echo "  - Ou utiliser: cat $OUTPUT_CSV | column -t -s','"
echo ""
