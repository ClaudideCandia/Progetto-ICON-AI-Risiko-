
% ==============================================================================
% SEZIONE 0: FATTI DINAMICI
% ==============================================================================
% Questi predicati verranno aggiunti/rimossi da Python durante il gioco.
% Formato: occupato(Territorio, Giocatore, NumeroArmate).
:- dynamic occupato/3.

% ==============================================================================
% SEZIONE 4: OBIETTIVI E VERIFICA
% ==============================================================================

% --- PREDICATI AUSILIARI (Helpers) ---\

% Conta quanti territori di un Continente NON sono posseduti dal Giocatore.
territori_mancanti_in_continente(Giocatore, Continente, NumMancanti) :-
    % Trova tutti i territori del continente che NON sono occupati dal Giocatore
    findall(T, (territorio(T, Continente), \+ occupato(T, Giocatore, _)), ListaMancanti),
    length(ListaMancanti, NumMancanti).

% Verifica se un giocatore possiede interamente un continente (0 mancanti).
possiede_continente(Giocatore, Continente) :-
    territori_mancanti_in_continente(Giocatore, Continente, 0).

% Trova il continente opzionale più facile da conquistare
miglior_terzo_continente(Giocatore, C1, C2, MigliorC, MinMancanti) :-
    findall(M-C, (
        territorio(_, C),           % Genera un continente C
        C \= C1, C \= C2,           % Assicurati che non sia uno dei due fissi
        territori_mancanti_in_continente(Giocatore, C, M) % Calcola quanto manca
    ), ListaCoppie),
    keysort(ListaCoppie, [MinMancanti-MigliorC | _]). % Ordina per mancanti crescenti e prendi il primo


% --- DEFINIZIONE OBIETTIVI ---\

% 1. OBIETTIVO: ELIMINA COLORE
obiettivo_raggiunto(_, distruggi_giocatore(Target)) :-
    \+ occupato(_, Target, _).

quanto_manca(_, distruggi_giocatore(Target), NumTerritoriRimasti) :-
    findall(T, occupato(T, Target, _), ListaTerritori),
    length(ListaTerritori, NumTerritoriRimasti).

% 2. OBIETTIVO: CONQUISTA 2 CONTINENTI SPECIFICI
obiettivo_raggiunto(Giocatore, conquista_continenti(C1, C2)) :-
    possiede_continente(Giocatore, C1),
    possiede_continente(Giocatore, C2).

quanto_manca(Giocatore, conquista_continenti(C1, C2), TotaleMancanti) :-
    territori_mancanti_in_continente(Giocatore, C1, M1),
    territori_mancanti_in_continente(Giocatore, C2, M2),
    TotaleMancanti is M1 + M2.

% 3. OBIETTIVO: CONQUISTA 2 CONTINENTI + 1 A SCELTA
obiettivo_raggiunto(Giocatore, conquista_continenti(C1, C2, terzo_continente)) :-
    possiede_continente(Giocatore, C1),
    possiede_continente(Giocatore, C2),
    territorio(_, C3),
    C3 \= C1, C3 \= C2,
    possiede_continente(Giocatore, C3).

quanto_manca(Giocatore, conquista_continenti(C1, C2, terzo_continente), TotaleMancanti) :-
    territori_mancanti_in_continente(Giocatore, C1, M1),
    territori_mancanti_in_continente(Giocatore, C2, M2),
    miglior_terzo_continente(Giocatore, C1, C2, _, M3),
    TotaleMancanti is M1 + M2 + M3.

% ### NUOVO: 4. OBIETTIVO NUMERICO (Es. 24 Territori) ###
obiettivo_raggiunto(Giocatore, conquista_territori(NumeroTarget)) :-
    findall(T, occupato(T, Giocatore, _), IMieiTerritori),
    length(IMieiTerritori, Posseduti),
    Posseduti >= NumeroTarget.

quanto_manca(Giocatore, conquista_territori(NumeroTarget), Mancanti) :-
    findall(T, occupato(T, Giocatore, _), IMieiTerritori),
    length(IMieiTerritori, Posseduti),
    (Posseduti >= NumeroTarget -> Mancanti = 0 ; Mancanti is NumeroTarget - Posseduti).

% ### NUOVO: 5. OBIETTIVO PRESIDIO (Es. 18 Territori con 2 Armate) ###
obiettivo_raggiunto(Giocatore, conquista_territori_armate(NumTarget, MinArmate)) :-
    findall(T, (
        occupato(T, Giocatore, ArmatePresenti),
        ArmatePresenti >= MinArmate
    ), TerritoriValidi),
    length(TerritoriValidi, Conteggio),
    Conteggio >= NumTarget.

quanto_manca(Giocatore, conquista_territori_armate(NumTarget, MinArmate), Mancanti) :-
    findall(T, (
        occupato(T, Giocatore, ArmatePresenti),
        ArmatePresenti >= MinArmate
    ), TerritoriValidi),
    length(TerritoriValidi, Conteggio),
    (Conteggio >= NumTarget -> Mancanti = 0 ; Mancanti is NumTarget - Conteggio).


% --- IDENTIFICAZIONE TERRITORI DELL'OBIETTIVO (SCOPE) ---\

% CASO A: Obiettivo Distruggi Giocatore
territorio_obiettivo(_, distruggi_giocatore(Nemico), Territorio) :-
    occupato(Territorio, Nemico, _).

% CASO B: Obiettivo Continenti
territorio_obiettivo(_, conquista_continenti(C1, C2), Territorio) :-
    (territorio(Territorio, C1) ; territorio(Territorio, C2)).

% CASO C: Obiettivo Conquista 2 Continenti + 1 A Scelta
territorio_obiettivo(Giocatore, conquista_continenti(C1, C2, terzo_continente), Territorio) :-
    (territorio(Territorio, C1) ; territorio(Territorio, C2)).

territorio_obiettivo(Giocatore, conquista_continenti(C1, C2, terzo_continente), Territorio) :-
    miglior_terzo_continente(Giocatore, C1, C2, MigliorC, _),
    territorio(Territorio, MigliorC).

% ### NUOVO: CASO D - Obiettivo Numerico (Tutto il mondo non mio è target) ###
territorio_obiettivo(Giocatore, conquista_territori(_), Territorio) :-
    \+ occupato(Territorio, Giocatore, _).

% ### NUOVO: CASO E - Obiettivo Presidio ###
% Sono obiettivi sia i territori nemici (da prendere)
% sia i MIEI territori che però hanno meno armate del richiesto (da rinforzare).
territorio_obiettivo(Giocatore, conquista_territori_armate(_, MinArmate), Territorio) :-
    \+ occupato(Territorio, Giocatore, _). % Territorio Nemico

territorio_obiettivo(Giocatore, conquista_territori_armate(_, MinArmate), Territorio) :-
    occupato(Territorio, Giocatore, Armate),
    Armate < MinArmate. % Territorio Mio ma debole
