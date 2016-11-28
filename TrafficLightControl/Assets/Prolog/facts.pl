/**
* Autoren: Tilo Zuelske, Hannes Boers und Laurens Gross
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Definition der Ampelanlagen                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('facts', [phase1/2,
                  phase2/2,
                  phase3/2,
                  phase4/2,
                  phase5/2,
                  phase6/2,
                  phase11/2,
                  phase12/2,
                  phase13/2,
                  phase14/2,
                  ausloeser/3  ]).


%Ampel Logik
% Gueltige Zustaende der Kreuzung nach STVO, für jede AmpelAnlage definierbar

%Phasen Ampelkreuzung A
%Aus Grunde dem heraus, dass die Originalen Phasen mit einer zusaetzlichen 1 zur Unterscheidung notiert wurden,
%uebernehmen auch wir diese Konvention
%phaseX(Kreuzung,[[Liste der grünen Ampeln],Phase,Zeitraum der Fahrfreigabe in Sekunden])
phase11(a,[[k9,k13,b3,fg9],phase11,35]).
phase12(a,[[b3,k9,fg9],phase12,10]).
phase13(a,[[fa11,fa10,k11,k10],phase13,20]).
phase14(a,[[fa11,fa10,k12],phase14,18]).

%Regeln Kreuzung A
%Welcher Ausloeser führt zu welcher Phase
%ausloeser(Kreuzung,Ausloeser,Liste der grünen Ampeln) fuehrt zu phaseX(Kreuzung,Liste der gruenen Ampeln)
ausloeser(a,keineAktion,GG):- phase11(a,GG).
ausloeser(a,b3,GG):- phase12(a,GG).
ausloeser(a,k10,GG):- phase13(a,GG).
ausloeser(a,fa10,GG):- phase14(a,GG).
ausloeser(a,k12,GG):- phase14(a,GG).

%Phasen Ampelkreuzung B
%phaseX(Kreuzung,[[Liste der gruenen Ampeln],Phase,Zeitraum der Fahrfreigabe in Sekunden])
phase1(b,[[fg8,fg6,k7,b1,k1,k5,k14,k2,k3,fg3,boomgate],phase1,35]).
phase2(b,[[fa7,k8,fg6,fa1,fa2,k6,k5,boomgate],phase2,10]).
phase3(b,[[fa7,fg8,fa1,fa2,k6,fa5,fa4,boomgate],phase3,9]).
phase4(b,[[fg8,k4,k7,b1,k1,k2,boomgate],phase4,5]).
phase5(b,[[k4,k7,b1,k2,boomgate],phase5,5]).
phase6(b,[[fg8,k7,b1,k1,k3,k2,fg3],phase6,15]).

%Regeln Kreuzung B
%Welcher Ausloeser fuehrt zu welcher Phase
%ausloeser(Kreuzung,Ausloeser,Liste der grünen Ampeln) führt zu phaseX(Kreuzung,Liste der grünen Ampeln)
ausloeser(b,keineAktion,GG):-phase1(b,GG).
ausloeser(b,fa4,GG):-phase3(b,GG).
ausloeser(b,fa1,GG):-phase2(b,GG).
ausloeser(b,k8,GG):-phase2(b,GG).
ausloeser(b,k6,GG):-phase2(b,GG).
ausloeser(b,k4,GG):-phase4(b,GG).
ausloeser(b,b1,GG):-phase5(b,GG).
ausloeser(b,boomgate,GG):-phase6(b,GG).
