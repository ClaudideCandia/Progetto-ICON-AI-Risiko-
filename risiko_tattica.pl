
% ==============================================================================
% SEZIONE TATTICA: FRONTE, RETROVIA, VULNERABILITÀ
% ==============================================================================

% Dichiariamo i fatti dinamici che verranno popolati da Python
:- dynamic occupato/3.
:- dynamic conquista_effettuata/1. % True se il giocatore ha preso un territorio nel turno
:- dynamic bonus_carte_attivo/1.   % True se il giocatore ha un Tris pronto da giocare
:- dynamic carte_giocatore/2.      % Definito in analisi_globale, ma utile dichiararlo qui per sicurezza

% --- HELPER: Analisi dei vicini ---
max_minaccia_vicina(Territorio, Giocatore, MaxArmateNemico) :-
    findall(Armate, (
        adiacente(Territorio, NemicoPos),
        occupato(NemicoPos, Avversario, Armate),
        Avversario \= Giocatore
    ), ListaArmateNemiche),
    (ListaArmateNemiche = [] -> MaxArmateNemico = 0 ; max_list(ListaArmateNemiche, MaxArmateNemico)).

% --- 1. IL FRONTE (Prima Linea) ---
fronte(Territorio, Giocatore) :-
    occupato(Territorio, Giocatore, _),
    adiacente(Territorio, Vicino),
    occupato(Vicino, Nemico, _),
    Giocatore \= Nemico.

% --- 2. LA RETROVIA (Seconda Linea) ---
retrovia(Territorio, Giocatore) :-
    occupato(Territorio, Giocatore, _),
    \+ fronte(Territorio, Giocatore).

% --- 3. TERRITORIO VULNERABILE ---
vulnerabile(Territorio, Giocatore) :-
    fronte(Territorio, Giocatore),
    occupato(Territorio, Giocatore, MieArmate),
    max_minaccia_vicina(Territorio, Giocatore, MaxNem),
    MaxNem > 0,
    Soglia is MaxNem / 2,
    MieArmate < Soglia.

% --- 4. TERRITORIO FORTE AL FRONTE ---
forte_al_fronte(Territorio, Giocatore) :-
    fronte(Territorio, Giocatore),
    occupato(Territorio, Giocatore, MieArmate),
    max_minaccia_vicina(Territorio, Giocatore, MaxNem),
    Soglia is MaxNem * 2,
    MieArmate > Soglia.

% --- 5. LOGICA "CARD FARMING" (Forza Attacco) ---
% L'agente deve forzare un attacco se:
% 1. NON ha ancora conquistato nulla in questo turno.
% 2. HA almeno un territorio forte al fronte (quindi ha risorse da spendere).
% 3. HA bisogno di carte (ne ha esattamente 2 OPPURE non ha un bonus attivo).
forza_attacco(Giocatore) :-
    \+ conquista_effettuata(Giocatore),
    forte_al_fronte(_, Giocatore), % Esiste almeno un territorio forte
    (carte_giocatore(Giocatore, 2) ; \+ bonus_carte_attivo(Giocatore)).

% ==============================================================================
% SEZIONE 6: GESTIONE CARTE (Tris)
% ==============================================================================

% Decide se è il momento giusto per giocare il tris.
% Non lo gioca se è in modalità 'consolidamento' e ha poche carte (aspetta un tris migliore),
% a meno che non sia costretto.

conviene_giocare_tris(Giocatore) :-
    bonus_carte_attivo(Giocatore), % Condizione necessaria: avere il tris
    (
        % A. CASO OBBLIGATO/EFFICIENZA: Ho 5 o più carte (rischio stallo o regola)
        (carte_giocatore(Giocatore, N), N >= 5);

        % B. CASO EMERGENZA: Rischio di morire, mi serve tutto l'aiuto possibile
        rischio_eliminazione(Giocatore);

        % C. CASO AGGRESSIVO: Voglio attaccare, mi servono munizioni
        modalita_attiva(Giocatore, dominio);

        % D. CASO STANDARD: Espansione (più truppe = più territori)
        modalita_attiva(Giocatore, espansione);

        % E. CASO RECUPERO: Sono indietro, devo colmare il gap
        in_svantaggio(Giocatore)
    ).

% Nota: Se sono in 'consolidamento' e ho < 5 carte, la regola fallisce (risparmio il tris).
