/**
* Autoren: Tilo Zuelske, Hannes Boers und Laurens Gross
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Definition der Ampelanlagen                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('facts', [sequence1/2,
                  sequence2/2,
                  sequence3/2,
                  sequence4/2,
                  sequence5/2,
                  sequence6/2,
                  sequence11/2,
                  sequence12/2,
                  sequence13/2,
                  sequence14/2,
                  trigger/3  ]).


%Ampel Logik
% Gueltige Zustaende der Kreuzung nach STVO, für jede Ampelanlage definierbar

%Sequencen Ampelkreuzung A
%Aus diesem Grunde heraus, dass die Originalen Sequencen mit einer zusaetzlichen 1 zur Unterscheidung notiert wurden,
%uebernehmen wir auch diese Konvention
%sequenceX(Kreuzung,[[Liste der grünen Ampeln],Phase,Zeitraum der Fahrfreigabe in Sekunden])
sequence11(a,[[k9,k13,b3,fg9],sequence11,35]).
sequence12(a,[[b3,k9,fg9],sequence12,10]).
sequence13(a,[[fa11,fa10,k11,k10],sequence13,20]).
sequence14(a,[[fa11,fa10,k12],sequence14,18]).

%Regeln Kreuzung A
%Welcher Trigger fuehrt zu welcher Sequence
%trigger(Kreuzung,Trigger,Liste der grünen Ampeln) fuehrt zu sequenceX(Kreuzung,Liste der gruenen Ampeln)
trigger(a,noAction,GG):- sequence11(a,GG).
trigger(a,b3,GG):- sequence12(a,GG).
trigger(a,k10,GG):- sequence13(a,GG).
trigger(a,fa10,GG):- sequence14(a,GG).
trigger(a,k12,GG):- sequence14(a,GG).

%Sequencen Ampelkreuzung B
%sequenceX(Kreuzung,[[Liste der gruenen Ampeln],Phase,Zeitraum der Fahrfreigabe in Sekunden])
sequence1(b,[[fg8,fg6,k7,b1,k1,k5,k14,k2,k3,fg3,boomgate],sequence1,35]).
sequence2(b,[[fa7,k8,fg6,fa1,fa2,k6,k5,boomgate],sequence2,10]).
sequence3(b,[[fa7,fg8,fa1,fa2,k6,fa5,fa4,boomgate],sequence3,9]).
sequence4(b,[[fg8,k4,k7,b1,k1,k2,boomgate,b1],sequence4,5]).
sequence5(b,[[k4,k7,b1,k2,boomgate],sequence5,5]).
sequence6(b,[[fg8,k7,b1,k1,k3,k2,fg3],sequence6,15]).

%Regeln Kreuzung B
%Welcher Trigger fuehrt zu welcher Phase
%trigger(Kreuzung,Trigger,Liste der grünen Ampeln) führt zu sequenceX(Kreuzung,Liste der grünen Ampeln)
trigger(b,noAction,GG):-sequence1(b,GG).
trigger(b,fa4,GG):-sequence3(b,GG).
trigger(b,fa1,GG):-sequence2(b,GG).
trigger(b,k8,GG):-sequence2(b,GG).
trigger(b,k6,GG):-sequence2(b,GG).
trigger(b,k4,GG):-sequence4(b,GG).
trigger(b,b1,GG):-sequence5(b,GG).
trigger(b,boomgate,GG):-sequence6(b,GG).
