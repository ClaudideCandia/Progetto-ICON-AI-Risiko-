
% ==============================================================================
% SEZIONE 1: GEOGRAFIA DELLA PLANCIA (RISIKO ITALIANO)
% ==============================================================================

% --- CONTINENTI E BONUS ARMATE ---
continente(nord_america, 5).
continente(sud_america, 2).
continente(europa, 5).
continente(africa, 3).
continente(asia, 7).
continente(oceania, 2).

% --- TERRITORI E APPARTENENZA ---
% Nord America
territorio(alaska, nord_america).
territorio(alberta, nord_america).
territorio(america_centrale, nord_america).
territorio(groenlandia, nord_america).
territorio(nord_ovest, nord_america).
territorio(ontario, nord_america).
territorio(quebec, nord_america).
territorio(stati_uniti_occidentali, nord_america).
territorio(stati_uniti_orientali, nord_america).
% Sud America
territorio(argentina, sud_america).
territorio(brasile, sud_america).
territorio(peru, sud_america).
territorio(venezuela, sud_america).
% Europa
territorio(gran_bretagna, europa).
territorio(islanda, europa).
territorio(europa_settentrionale, europa).
territorio(europa_occidentale, europa).
territorio(europa_meridionale, europa).
territorio(scandinavia, europa).
territorio(ucraina, europa).
% Africa
territorio(congo, africa).
territorio(africa_orientale, africa).
territorio(egitto, africa).
territorio(madagascar, africa).
territorio(nord_africa, africa).
territorio(sud_africa, africa).
% Asia
territorio(afghanistan, asia).
territorio(cina, asia).
territorio(india, asia).
territorio(cita, asia).
territorio(giappone, asia).
territorio(jacuzia, asia).
territorio(kamchatka, asia).
territorio(medio_oriente, asia).
territorio(mongolia, asia).
territorio(siam, asia).
territorio(siberia, asia).
territorio(urali, asia).
% Oceania
territorio(australia_orientale, oceania).
territorio(australia_occidentale, oceania).
territorio(indonesia, oceania).
territorio(nuova_guinea, oceania).

% --- PROPRIETÀ SPECIALE: TERRITORI INTERCONTINENTALI ---
% Territori che fungono da ponte verso altri continenti.

% Europa
intercontinentale(ucraina).
intercontinentale(islanda).
intercontinentale(europa_meridionale).
intercontinentale(europa_occidentale).

% Nord America
intercontinentale(alaska).
intercontinentale(groenlandia).
intercontinentale(america_centrale).

% Oceania
intercontinentale(indonesia).

% Africa
intercontinentale(nord_africa).
intercontinentale(egitto).

% Asia
intercontinentale(medio_oriente).
intercontinentale(afghanistan).
intercontinentale(urali).
intercontinentale(kamchatka).
intercontinentale(siam).


% --- CONFINI (ADJACENCY LIST) ---
% Nord America
confina(alaska, kamchatka).
confina(alaska, alberta).
confina(alaska, nord_ovest).
confina(alberta, nord_ovest).
confina(alberta, ontario).
confina(alberta, stati_uniti_occidentali).
confina(america_centrale, stati_uniti_occidentali).
confina(america_centrale, stati_uniti_orientali).
confina(america_centrale, venezuela).
confina(groenlandia, islanda).
confina(groenlandia, nord_ovest).
confina(groenlandia, ontario).
confina(groenlandia, quebec).
confina(nord_ovest, ontario).
confina(ontario, quebec).
confina(ontario, stati_uniti_occidentali).
confina(ontario, stati_uniti_orientali).
confina(quebec, stati_uniti_orientali).
confina(stati_uniti_occidentali, stati_uniti_orientali).
% Sud America
confina(argentina, brasile).
confina(argentina, peru).
confina(brasile, nord_africa).
confina(brasile, peru).
confina(brasile, venezuela).
confina(peru, venezuela).
% Europa
confina(gran_bretagna, islanda).
confina(gran_bretagna, europa_settentrionale).
confina(gran_bretagna, europa_occidentale).
confina(gran_bretagna, scandinavia).
confina(islanda, scandinavia).
confina(europa_settentrionale, europa_occidentale).
confina(europa_settentrionale, europa_meridionale).
confina(europa_settentrionale, ucraina).
confina(europa_settentrionale, scandinavia).
confina(europa_occidentale, europa_meridionale).
confina(europa_occidentale, nord_africa).
confina(europa_meridionale, egitto).
confina(europa_meridionale, nord_africa).
confina(europa_meridionale, ucraina).
confina(scandinavia, ucraina).
confina(ucraina, urali).
confina(ucraina, afghanistan).
% Africa
confina(congo, africa_orientale).
confina(congo, nord_africa).
confina(congo, sud_africa).
confina(africa_orientale, egitto).
confina(africa_orientale, madagascar).
confina(africa_orientale, nord_africa).
confina(africa_orientale, sud_africa).
confina(egitto, nord_africa).
confina(madagascar, sud_africa).
% Asia (Core modificato secondo richieste precedenti)
confina(medio_oriente, egitto).
confina(medio_oriente, ucraina).
confina(medio_oriente, europa_meridionale).
confina(medio_oriente, afghanistan).
confina(medio_oriente, cina).
confina(medio_oriente, india).
confina(afghanistan, cina).
confina(afghanistan, india).
confina(afghanistan, urali).
confina(cina, india).
confina(cina, mongolia).
confina(cina, siam).
confina(cina, siberia).
confina(cina, urali).
confina(cita, giappone).
confina(cita, kamchatka).
confina(cita, mongolia).
confina(cita, siberia).
confina(cita, jacuzia).
confina(giappone, kamchatka).
confina(giappone, mongolia).
confina(jacuzia, kamchatka).
confina(jacuzia, siberia).
confina(mongolia, kamchatka).
confina(mongolia, siberia).
confina(siam, india).
confina(siam, indonesia).
confina(siberia, urali).
% Oceania
confina(australia_orientale, australia_occidentale).
confina(australia_orientale, nuova_guinea).
confina(australia_occidentale, indonesia).
confina(australia_occidentale, nuova_guinea).
confina(indonesia, nuova_guinea).

% Regola bidirezionale
adiacente(X, Y) :- confina(X, Y).
adiacente(X, Y) :- confina(Y, X).
