
% ==============================================================================
% SEZIONE 6: COMPORTAMENTO E PRIORITÀ STRATEGICHE
% ==============================================================================
% Dipendenze logiche (devono essere caricate le altre KB):
% - rischio_eliminazione/1, in_svantaggio/1, dominante/1 (da analisi_globale)
% - vulnerabile/2, forte_al_fronte/2 (da tattica)

% --- 1. DETERMINAZIONE DELLA MODALITÀ DI GIOCO ---
% Definisce "l'umore" dell'agente in base allo stato globale.

modalita_attiva(Giocatore, sopravvivenza) :-
    rischio_eliminazione(Giocatore).

modalita_attiva(Giocatore, consolidamento) :-
    in_svantaggio(Giocatore),
    \+ rischio_eliminazione(Giocatore).

modalita_attiva(Giocatore, dominio) :-
    dominante(Giocatore).

% Default: Se non è in nessuno degli stati sopra, è in espansione
modalita_attiva(Giocatore, espansione) :-
    normale(Giocatore).


% --- 2. ASSEGNAZIONE PESI ALLE AZIONI (Policy) ---
% peso_azione(Modalita, TipoAzione, Peso).
% I pesi vanno da 0 a 100.

% --- A. MODALITÀ SOPRAVVIVENZA (Tartaruga) ---
% Obiettivo: Non morire. Difendere tutto. Non attaccare a meno che sia gratis.
peso_azione(sopravvivenza, rinforzo_difensivo, 100).   % Massima priorità
peso_azione(sopravvivenza, rinforzo_offensivo, 10).    % Inutile ora
peso_azione(sopravvivenza, attacco_sicuro, 20).        % Solo se garantito
peso_azione(sopravvivenza, attacco_rischioso, 0).      % MAI
peso_azione(sopravvivenza, spostamento_fronte, 80).    % Portare tutto al fronte

% --- B. MODALITÀ CONSOLIDAMENTO (Costruttore) ---
% Obiettivo: Uscire dallo svantaggio prendendo continenti piccoli.
peso_azione(consolidamento, rinforzo_difensivo, 80).
peso_azione(consolidamento, rinforzo_offensivo, 40).
peso_azione(consolidamento, attacco_sicuro, 60).
peso_azione(consolidamento, attacco_rischioso, 10).
peso_azione(consolidamento, spostamento_fronte, 60).

% --- C. MODALITÀ ESPANSIONE (Equilibrato) ---
% Obiettivo: Gioco standard. Bilanciamento tra obiettivi e difesa.
peso_azione(espansione, rinforzo_difensivo, 50).
peso_azione(espansione, rinforzo_offensivo, 70).       % Preparare attacchi è importante
peso_azione(espansione, attacco_sicuro, 80).           % Prendere territori facili
peso_azione(espansione, attacco_rischioso, 30).        % Si può osare ogni tanto
peso_azione(espansione, spostamento_fronte, 50).

% --- D. MODALITÀ DOMINIO (Schiacciasassi) ---
% Obiettivo: Annientare. Spingere tutto sull'attacco.
peso_azione(dominio, rinforzo_difensivo, 20).          % La miglior difesa è l'attacco
peso_azione(dominio, rinforzo_offensivo, 100).         % Caricare i cannoni
peso_azione(dominio, attacco_sicuro, 100).             % Mangiare tutto
peso_azione(dominio, attacco_rischioso, 60).           % Osare per chiudere
peso_azione(dominio, spostamento_fronte, 40).


% --- 3. REGOLA INTERFACCIA (API) ---
% Questa è la regola che Python interrogherà.
% Restituisce la priorità di una specifica azione per il giocatore corrente.

ottieni_priorita(Giocatore, TipoAzione, Peso) :-
    modalita_attiva(Giocatore, Modalita),
    peso_azione(Modalita, TipoAzione, Peso).

% Debug: Per capire in che modalità è l'agente
debug_modalita(Giocatore, Modalita) :-
    modalita_attiva(Giocatore, Modalita).
