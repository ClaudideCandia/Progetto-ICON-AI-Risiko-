
% ==============================================================================
% SEZIONE 5: ANALISI GLOBALE E STATO GIOCATORI
% ==============================================================================
% Dipendenze:
% - occupato/3 (Dinamico)
% - carte_giocatore/2 (Dinamico)
% - intercontinentale/1 (da risiko_geografia.pl)
% - territorio/2 (da risiko_geografia.pl)
% - vulnerabile/2 (da risiko_tattica.pl)

:- dynamic occupato/3.
:- dynamic carte_giocatore/2.

% --- CALCOLO DEL FATTORE MOLTIPLICATIVO CARTE ---
fattore_carte(NumCarte, 1.0) :- NumCarte =< 2.
fattore_carte(NumCarte, 2.0) :- NumCarte >= 3, NumCarte =< 4.
fattore_carte(NumCarte, 3.0) :- NumCarte >= 5.

% --- CALCOLO DEL PUNTEGGIO DI POTENZA ---
calcola_punteggio(Giocatore, ScoreFinale) :-
    occupato(_, Giocatore, _),

    % 1. Calcolo Score Base
    findall(T, (occupato(T, Giocatore, _), \+ intercontinentale(T)), ListaNorm), length(ListaNorm, NumNorm),
    findall(Tp, (occupato(Tp, Giocatore, _), intercontinentale(Tp)), ListaPonte), length(ListaPonte, NumPonte),
    findall(A, occupato(_, Giocatore, A), ListaArmate), sum_list(ListaArmate, TotArmate),
    % Qui chiamiamo la regola che prima falliva
    findall(C, possiede_intero_continente_check(Giocatore, C), ListaCont), length(ListaCont, NumCont),

    BaseScore is (NumNorm * 1.0) + (NumPonte * 1.5) + (TotArmate * 1.5) + (NumCont * 2.0),

    % 2. Recupero Carte e Applicazione Moltiplicatore
    (carte_giocatore(Giocatore, N) -> NumCarte = N ; NumCarte = 0),
    fattore_carte(NumCarte, Moltiplicatore),

    ScoreFinale is BaseScore * Moltiplicatore.

% --- Helper CORRETTO per verificare possesso continente ---
possiede_intero_continente_check(Giocatore, Continente) :-
    continente(Continente, _),
    \+ (territorio(T, Continente), \+ occupato(T, Giocatore, _)).


% --- DEFINIZIONE DEGLI STATI GIOCATORE ---

dominante(Giocatore) :-
    calcola_punteggio(Giocatore, MioScore),
    forall((occupato(_, Altro, _), Altro \= Giocatore),
           (calcola_punteggio(Altro, AltroScore), MioScore > AltroScore)).

in_svantaggio(Giocatore) :-
    calcola_punteggio(Giocatore, MioScore),
    forall((occupato(_, Altro, _), Altro \= Giocatore),
           (calcola_punteggio(Altro, AltroScore), MioScore < AltroScore)).

normale(Giocatore) :-
    occupato(_, Giocatore, _),
    \+ dominante(Giocatore),
    \+ in_svantaggio(Giocatore).

rischio_eliminazione(Giocatore) :-
    occupato(_, Giocatore, _),
    findall(T, occupato(T, Giocatore, _), Territori),
    length(Territori, N),
    N < 3.


% --- ANALISI STATO CONTINENTI ---

continente_conteso(Continente) :-
    continente(Continente, _),
    setof(Giocatore, T^(territorio(T, Continente), occupato(T, Giocatore, _)), ListaProprietari),
    length(ListaProprietari, N),
    N > 1.

continente_fortezza(Continente, Giocatore) :-
    possiede_intero_continente_check(Giocatore, Continente),
    forall(
        (territorio(T, Continente), intercontinentale(T)),
        \+ vulnerabile(T, Giocatore)
    ).
