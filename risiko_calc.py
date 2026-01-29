#!/usr/bin/env python3
import itertools
import sys
import time

# =============================================================================
# LOGICA CORE (TUA CLASSE)
# =============================================================================

class RiskProbabilityCalculator:
    def __init__(self):
        self.bmem = {} # Battle memoization
        self.omem = {} # Outcome memoization
        self.tmem = {} # Tour memoization
        # Pre-calcola la matrice delle probabilità dei dadi all'avvio
        self.probs = self._precompute_dice_probabilities()

    def _precompute_dice_probabilities(self):
        """
        Genera la matrice delle probabilità per scontri fino a 3v3.
        Regole Italiane: Difensore vince i pareggi, max 3 dadi per parte.
        """
        # probs[att_dadi][def_dadi] = lista probabilità perdite attaccante [0, 1, 2, 3]
        probs = [[None for _ in range(4)] for _ in range(4)]
        dice_sides = [1, 2, 3, 4, 5, 6]

        for n_att in range(1, 4):
            for n_def in range(1, 4):
                att_rolls = list(itertools.product(dice_sides, repeat=n_att))
                def_rolls = list(itertools.product(dice_sides, repeat=n_def))
                total = len(att_rolls) * len(def_rolls)
                attacker_losses_count = [0] * (min(n_att, n_def) + 1)

                for a_roll in att_rolls:
                    for d_roll in def_rolls:
                        a_sorted = sorted(a_roll, reverse=True)
                        d_sorted = sorted(d_roll, reverse=True)
                        losses = 0
                        for k in range(min(n_att, n_def)):
                            # Regola: Difensore vince i pareggi
                            if d_sorted[k] >= a_sorted[k]:
                                losses += 1
                        attacker_losses_count[losses] += 1

                probs[n_att][n_def] = [c / total for c in attacker_losses_count]
        return probs

    def outcome_prob(self, attackers, defenders, arem=1, drem=0):
        """
        Calcola ricorsivamente la probabilità che uno scontro finisca
        esattamente con 'arem' attaccanti e 'drem' difensori rimasti.
        """
        if attackers < arem or defenders < drem: return 0.0
        if defenders == drem: return 1.0 if attackers == arem else 0.0

        h = (attackers, defenders, arem, drem)
        if h in self.omem: return self.omem[h]

        # Determina numero dadi (Attaccante lascia sempre 1 armata di base sul territorio di lancio)
        if attackers >= 4: n_att = 3
        elif attackers == 3: n_att = 2
        elif attackers == 2: n_att = 1
        else: return 0.0 # Non può attaccare

        n_def = min(3, defenders) # Difesa italiana: max 3 dadi

        outcome_probs = self.probs[n_att][n_def]
        val = 0.0
        comparisons = min(n_att, n_def)

        for att_lost, prob in enumerate(outcome_probs):
            if prob > 0:
                def_lost = comparisons - att_lost
                val += prob * self.outcome_prob(attackers - att_lost, defenders - def_lost, arem, drem)

        self.omem[h] = val
        return val

    def tour_prob(self, attackers, darmies, tindex=0, fortify=None):
        """
        Calcola la probabilità di vincere una sequenza di territori (darmies).
        fortify: lista di quante armate lasciare su ogni territorio conquistato.
        """
        if fortify is None: fortify = [1] * len(darmies)

        if tindex == len(darmies): return 1.0 # Tutti i territori conquistati
        if tindex == 0: self.tmem = {} # Reset cache per nuova catena

        h = (attackers, tindex)
        if h in self.tmem: return self.tmem[h]

        # Armate disponibili per questo scontro:
        current_attackers = attackers
        enemy_armies = darmies[tindex]

        # Check rapido: Ho abbastanza truppe per coprire i presidi futuri?
        future_fortify_cost = sum(fortify[i] for i in range(tindex, len(darmies)))
        if current_attackers <= future_fortify_cost: return 0.0

        val = 0.0
        # Somma su tutti i possibili esiti positivi (sopravvissuti > 0)
        # i = armate sopravvissute DOPO aver sconfitto questo nemico
        for i in range(1, current_attackers + 1):
             prob_outcome = self.outcome_prob(current_attackers, enemy_armies, i)
             if prob_outcome > 0:
                 # Passo successivo: Sottraggo il presidio (fortify[tindex]) da lasciare QUI
                 next_attackers = i - fortify[tindex]
                 if next_attackers > 0:
                     val += prob_outcome * self.tour_prob(next_attackers, darmies, tindex+1, fortify)

        self.tmem[h] = val
        return val

# =============================================================================
# INTERFACCIA UTENTE TERMINALE
# =============================================================================

def get_int_input(prompt, min_val=0):
    while True:
        try:
            val = int(input(prompt))
            if val < min_val:
                print(f"Inserire un numero maggiore o uguale a {min_val}.")
                continue
            return val
        except ValueError:
            print("Errore: Inserire un numero intero valido.")

def main():
    print("\n" + "="*60)
    print("   RISK BATTLE SIMULATOR - CALCOLATORE CATENA DI ATTACCO")
    print("   Regole: RisiKo! Italiano (Difensore vince pareggi, 3 dadi)")
    print("="*60 + "\n")

    # 1. Setup Iniziale
    print("--- Configurazione Attaccante ---")
    total_troops = get_int_input("Inserisci il numero TOTALE delle tue armate nel territorio di partenza: ", 2)
    
    # 2. Definizione del Percorso
    chain_length = get_int_input("\nQuanti territori vuoi conquistare in sequenza? ", 1)
    
    territories = []
    defenders_list = []
    fortify_list = []

    print("\n--- Definizione Percorso ---")
    for i in range(chain_length):
        print(f"\n[Territorio {i+1}]")
        t_name = input(f"Nome del territorio {i+1} (opzionale): ").strip()
        if not t_name: t_name = f"Territorio {i+1}"
        
        defs = get_int_input(f"Quante armate nemiche ci sono in '{t_name}'? ", 1)
        
        # Logica del "fortify": quante ne lasci indietro DOPO aver vinto, 
        # sul territorio da cui hai lanciato l'attacco per muoverti qui.
        prompt_fort = f"Quante armate lasci a presidio sul territorio PRECEDENTE dopo la conquista? (Default 1): "
        fort = input(prompt_fort)
        fort = int(fort) if fort.isdigit() and int(fort) > 0 else 1
        
        territories.append(t_name)
        defenders_list.append(defs)
        fortify_list.append(fort)

    # 3. Calcolo
    print("\n" + "-"*60)
    print("Calcolo delle probabilità in corso...")
    calculator = RiskProbabilityCalculator()
    
    # Le armate mobili sono quelle totali meno 1 (che deve rimanere di base sul territorio iniziale)
    # Nota: nel codice originale `tour_prob` sottrae i `fortify` successivi. 
    # Ma per l'inizio, passiamo le truppe mobili.
    mobile_troops = total_troops - 1 

    print("\nRISULTATI ANALISI:")
    print(f"{'STEP':<25} | {'DIFENSORI':<10} | {'PRESIDIO':<10} | {'PROBABILITÀ CUMULATIVA':<20}")
    print("-" * 75)

    final_prob = 0.0

    for i in range(1, chain_length + 1):
        # Analizziamo la catena parziale fino allo step i
        partial_defs = defenders_list[:i]
        partial_fortify = fortify_list[:i]
        
        # Calcolo probabilità per questa catena
        prob = calculator.tour_prob(mobile_troops, partial_defs, 0, partial_fortify)
        final_prob = prob
        
        current_name = territories[i-1]
        current_defs = defenders_list[i-1]
        current_fort = fortify_list[i-1]
        
        perc_str = f"{prob * 100:.2f}%"
        
        # Colora output se supportato (semplice check visivo)
        if prob > 0.8: perc_str += " (Ottimo)"
        elif prob > 0.5: perc_str += " (Rischioso)"
        else: perc_str += " (Suicida)"

        print(f"{current_name:<25} | {current_defs:<10} | {current_fort:<10} | {perc_str}")

    print("-" * 75)
    print(f"Probabilità TOTALE di completare l'intero percorso: {final_prob * 100:.4f}%")
    print("="*60 + "\n")

if __name__ == "__main__":
    main()
