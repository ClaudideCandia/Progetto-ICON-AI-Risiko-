<img width="2816" height="1536" alt="Gemini_Generated_Image_3thwuu3thwuu3thw" src="https://github.com/user-attachments/assets/3b76c007-c265-467e-96aa-2f3260767429" />

# Risiko AI Player python+prolog

Questo repository contiene un progetto ibrido Python + Prolog per la simulazione e l'analisi strategica del gioco da tavolo Risiko (regolamento italiano).

Il progetto combina un motore di gioco scritto in Python con una base di conoscenza logica in SWI-Prolog, utilizzata per modellare il ragionamento tattico e strategico degli agenti AI (valutazione del fronte, gestione carte, scelta degli obiettivi).

## Caratteristiche Principali

* Motore di Gioco (Python)
    * Gestione completa della plancia e del grafo dei territori tramite NetworkX.
    * Implementazione delle fasi di gioco: Rinforzo, Attacco (con simulazione dadi), Spostamento.
    * Gestione del mazzo di carte e logica dello scambio (Tris).

* Intelligenza Artificiale (SWI-Prolog)
    * Geografia: Definizione logica di continenti, territori e adiacenze.
    * Tattica: Identificazione di fronti, retrovie e territori vulnerabili.
    * Analisi Globale: Calcolo punteggi di potenza e stati del giocatore (Dominante, In svantaggio, Rischio eliminazione).
    * Comportamento: Cambio dinamico della modalità di gioco (Sopravvivenza, Espansione, Consolidamento, Dominio).

## Requisiti

Il progetto è progettato per l'esecuzione in ambiente Linux (es. Google Colab o Ubuntu) in quanto richiede l'installazione di sistema di SWI-Prolog.

* Python 3.x
* SWI-Prolog (Core engine)
* Librerie Python:
    * pyswip (Bridge Python-Prolog)
    * networkx (Gestione grafi)
    * matplotlib (Visualizzazione)

## Installazione

Per eseguire il progetto in un ambiente locale basato su Debian/Ubuntu:

1. Installa SWI-Prolog:
   sudo apt-get update -y
   sudo apt-get install -y swi-prolog

2. Installa le dipendenze Python:
   pip install pyswip networkx matplotlib

Nota: Se utilizzi Google Colab, la configurazione dell'ambiente viene eseguita automaticamente nella prima cella del notebook.

## Struttura del Progetto

Il codice è organizzato per generare dinamicamente i file Prolog necessari.

### File Prolog (Knowledge Base)
* risiko_geografia.pl: Definisce la mappa (Continenti, Territori, Adiacenze).
* risiko_obbiettivi.pl: Logica per verificare il raggiungimento degli obiettivi.
* risiko_tattica.pl: Regole per l'analisi tattica (es. controllo del fronte, vulnerabilità).
* risiko_analisi_globale.pl: Analisi dello stato complessivo della partita.
* risiko_comportamento.pl: Definisce le priorità delle azioni in base alla modalità dell'agente.

### Classi Python
* RiskBoard: Rappresenta la mappa come un grafo networkx, gestisce proprietari e armate.
* RiskGame: Gestisce il flusso della partita, i turni, le battaglie e l'interazione con le carte.

## Utilizzo

1. Aprire il notebook notebook_progetto_icon_finale.ipynb.
2. Eseguire le celle in ordine sequenziale per:
    * Installare le dipendenze.
    * Generare i file .pl nella directory di lavoro.
    * Inizializzare le classi del motore di gioco.
3. Utilizzare la funzione visualize_board(RiskBoard()) per visualizzare la mappa iniziale.

---
Progetto sviluppato per il corso di ICON (Intelligenza Computazionale).

## 📊 Esempio di Visualizzazione

Il progetto include funzioni di visualizzazione che colorano i territori in base al continente o al giocatore che li possiede, mostrando graficamente gli equilibri di potere sulla plancia.
<img width="1957" height="966" alt="risk_board" src="https://github.com/user-attachments/assets/0ad274bf-358a-4c46-bf3e-a8a0c328957c" />

---
*Progetto sviluppato per il corso di ICON (Intelligenza Computazionale).*
