#!/bin/bash

# Script pour exécuter tous les tests localement avant commit/push
# Usage: ./run_tests.sh

set -e  # Arrêter en cas d'erreur

echo "======================================"
echo "  Tests Puissance 4 - Suite complète"
echo "======================================"
echo ""

# Couleurs pour l'affichage
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Fonction pour afficher les résultats
print_result() {
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ $1 réussi${NC}"
        return 0
    else
        echo -e "${RED}✗ $1 échoué${NC}"
        exit 1
    fi
}

# 1) Tests game.pl
echo -e "${BLUE}[1/5] Tests game.pl...${NC}"
swipl -s main.pl -g "consult('test_game.pl'), run_tests, halt"
print_result "Tests game.pl"
echo ""

# 2) Tests minimax/alphabeta
echo -e "${BLUE}[2/5] Tests minimax/alphabeta...${NC}"
swipl -s main.pl -g "consult('test_minimax.pl'), run_tests, halt"
print_result "Tests minimax/alphabeta"
echo ""

# 3) Vérification chargement modules
echo -e "${BLUE}[3/5] Vérification chargement modules...${NC}"
swipl -s main.pl -g "use_module(game), use_module(simulator), use_module(ia_minimax), use_module(ia_alphabeta), use_module(ia_negamax), use_module(benchmark), use_module(profiling), writeln('OK'), halt" > /dev/null
print_result "Chargement modules"
echo ""

# 4) Test de performance (rapide)
echo -e "${BLUE}[4/5] Test de performance (profondeur 4)...${NC}"
swipl -s main.pl -g "use_module(profiling), init_game(B), profiling:compare_node_efficiency(B, '\U0001F534', 4), halt" > /dev/null
print_result "Performance profondeur 4"
echo ""

# 5) Vérification syntaxe de tous les fichiers Prolog
echo -e "${BLUE}[5/5] Vérification syntaxe...${NC}"
for file in *.pl; do
    if [ -f "$file" ]; then
        swipl -g "consult('$file'), halt" -t 'halt(1)' 2>/dev/null
        if [ $? -eq 0 ]; then
            echo "  ✓ $file"
        else
            echo -e "  ${RED}✗ $file${NC}"
            exit 1
        fi
    fi
done
print_result "Vérification syntaxe"
echo ""

echo "======================================"
echo -e "${GREEN}  ✓ Tous les tests sont passés !${NC}"
echo "======================================"
echo ""
echo "Vous pouvez commit et push en toute sécurité."
