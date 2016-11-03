% Autoren: Tilo Zuelske und Hannes Boers
% Datum: 27.10.2016
:- style_check(-discontiguous).
%Ampel Logik
% Gültige Zustände der Kreuzung nach STVO, für jede AmpelAnlage definierbar

%Phasen Ampelkreuzung A
%Aus Grunde dem heraus, dass die Originalen Phasen mit einer zusätzlichen 1 zur Unterscheidung notiert wurden,
%übernehmen auch wir diese Konvention
phase11(a,[gruen(k9),gruen(k13),gruen(b4),gruen(h6),gruen(fg9)]).
phase12(a,[gruen(b3),gruen(k9),gruen(fg9)]).
phase13(a,[gruen(h5),gruen(f11),gruen(fa10),gruen(k11),gruen(k10)]).
phase14(a,[gruen(h5),gruen(fa11),gruen(fa10),gruen(k12)]).

%Regeln Kreuzung A
ausloeser(a,keineAktion,GG):-phase11(a,GG).
ausloeser(a,b3,GG):- phase12(a,GG).
ausloeser(a,k10,GG):-phase13(a,GG).
ausloeser(a,f10,GG):- phase14(a,GG).
ausloeser(a,k12,GG):- phase14(a,GG).


%Phasen Ampelkreuzung B
phase1(b,[gruen(h3),gruen(fg8),gruen(fg6),gruen(k7),gruen(b2),gruen(k1),gruen(k5),gruen(k14),gruen(k2),gruen(k3),gruen(fg3),gruen(h1)]).
phase2(b,[gruen(fa7),gruen(k8),gruen(fg6),gruen(fa1),gruen(fa2),gruen(k6),gruen(k5)]).
phase3(b,[gruen(fa7),gruen(fg5),gruen(fg8),gruen(fa1),gruen(fa2),gruen(h4),gruen(k6),gruen(h2),gruen(fa5),gruen(fa4)]).
phase4(b,[gruen(h3),gruen(fg8),gruen(k4),gruen(k7),gruen(b2),gruen(k1),gruen(k2)]).
phase5(b,[gruen(k4),gruen(k7),gruen(b1),gruen(k2),gruen(b2)]).
phase6(b,[gruen(h3),gruen(fg8),gruen(fg5),gruen(k7),gruen(b2),gruen(k1),gruen(k3),gruen(k2),gruen(fg3),gruen(h1)]).

%Regeln Kreuzung B

%erfragen der nächsten Ampelphase durch übergabe des Impulsgebers
getnextPhase(a,MomentanePhase,Ausloeserampel,Gruenegesamt):-checkifzulaessig(a,MomentanePhase,Ausloeserampel,Gruenegesamt),!.
getnextPhase(b,MomentanePhase,Ausloeserampel,Gruenegesamt):-checkifzulaessig(b,MomentanePhase,Ausloeserampel,Gruenegesamt),!.


%erfragen ob die Phasenaenderung zulässig ist für die jeweilige Ampelkreuzung
%Kreuzung A
checkifzulaessig(a,phase14,k10,[]).
checkifzulaessig(a,_,Ausloeser,GG):-ausloeser(a,Ausloeser,GG).

%Kreuzung B
%checkifzulaessig(b,MomentanePhase,Ausloeser):-



