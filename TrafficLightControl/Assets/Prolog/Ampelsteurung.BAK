% Autoren: Tilo Zuelske und Hannes Boers
% Datum: 27.10.2016
:- style_check(-discontiguous).
:-set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).
%Ampel Logik
% Gültige Zustände der Kreuzung nach STVO, für jede AmpelAnlage definierbar

%Phasen Ampelkreuzung A
%Aus Grunde dem heraus, dass die Originalen Phasen mit einer zusätzlichen 1 zur Unterscheidung notiert wurden,
%übernehmen auch wir diese Konvention
phase11(a,[gruen(k9),gruen(k13),gruen(b4),gruen(h6),gruen(fg9),phase11]).
phase12(a,[gruen(b3),gruen(k9),gruen(fg9),phase12]).
phase13(a,[gruen(h5),gruen(fa11),gruen(fa10),gruen(k11),gruen(k10),phase13]).
phase14(a,[gruen(h5),gruen(fa11),gruen(fa10),gruen(k12),phase14]).

%Regeln Kreuzung A
ausloeser(a,keineAktion,GG):-phase11(a,GG).
ausloeser(a,b3,GG):- phase12(a,GG).
ausloeser(a,k10,GG):-phase13(a,GG).
ausloeser(a,fa10,GG):- phase14(a,GG).
ausloeser(a,k12,GG):- phase14(a,GG).
ausloeser(a,fa11,_):-!,
                     write('use fa10 instead'),
                     fail.


%Phasen Ampelkreuzung B
phase1(b,[gruen(h3),gruen(fg8),gruen(fg6),gruen(k7),gruen(b2),gruen(k1),gruen(k5),gruen(k14),gruen(k2),gruen(k3),gruen(fg3),gruen(h1),phase1]).
phase2(b,[gruen(fa7),gruen(k8),gruen(fg6),gruen(fa1),gruen(fa2),gruen(k6),gruen(k5),phase2]).
phase3(b,[gruen(fa7),gruen(fg5),gruen(fg8),gruen(fa1),gruen(fa2),gruen(h4),gruen(k6),gruen(h2),gruen(fa5),gruen(fa4),phase3]).
phase4(b,[gruen(h3),gruen(fg8),gruen(k4),gruen(k7),gruen(b2),gruen(k1),gruen(k2),phase4]).
phase5(b,[gruen(k4),gruen(k7),gruen(b1),gruen(k2),gruen(b2),phase5]).
phase6(b,[gruen(h3),gruen(fg8),gruen(fg5),gruen(k7),gruen(b2),gruen(k1),gruen(k3),gruen(k2),gruen(fg3),gruen(h1),phase6]).

%Regeln Kreuzung B
ausloeser(b,keineAktion,GG):-phase1(b,GG).
ausloeser(b,fa4,GG):-phase3(b,GG).
ausloeser(b,fa1,GG):-phase2(b,GG).
ausloeser(b,k8,GG):-phase2(b,GG).
ausloeser(b,k6,GG):-phase2(b,GG).
ausloeser(b,k4,GG):-phase4(b,GG).
ausloeser(b,b1,GG):-phase5(b,GG).
ausloeser(b,schranke,GG):-phase6(b,GG).

%erfragen der nächsten Ampelphase durch übergabe des Impulsgebers
getnextPhase(a,MomentanePhase,Ausloeserampel,Gruenegesamt):-checkifzulaessig(a,MomentanePhase,Ausloeserampel,Gruenegesamt),!.
getnextPhase(b,MomentanePhase,Ausloeserampel,Gruenegesamt):-checkifzulaessig(b,MomentanePhase,Ausloeserampel,Gruenegesamt),!.


%erfragen ob die Phasenaenderung zulässig ist für die jeweilige Ampelkreuzung
%Kreuzung A
%nicht zulässige Übergänge
checkifzulaessig(a,phase14,k10,[]).
%zulässig
checkifzulaessig(a,_,Ausloeser,GG):-ausloeser(a,Ausloeser,GG).

%Kreuzung B
%nicht zulässige Übergänge
checkifzulaessig(b,phase3,keineAktion,[]).
checkifzulaessig(b,phase3,fa1,[]).
checkifzulaessig(b,phase3,k8,[]).
checkifzulaessig(b,phase3,k6,[]).
checkifzulaessig(b,phase2,fa4,[]).
checkifzulaessig(b,phase5,fa1,[]).
checkifzulaessig(b,phase5,k8,[]).
checkifzulaessig(b,phase5,k6,[]).
checkifzulaessig(b,phase5,k4,[]).
checkifzulaessig(b,phase4,b1,[]).
checkifzulaessig(b,phase6,k4,[]).
checkifzulaessig(b,phase3,schranke,[]).
checkifzulaessig(b,phase2,keineAktion,[]).
checkifzulaessig(b,phase2,schranke,[]).
checkifzulaessig(b,phase5,schranke,[]).
checkifzulaessig(b,phase4,fa4,[]).
checkifzulaessig(b,phase4,fa1,[]).
checkifzulaessig(b,phase4,k6,[]).
checkifzulaessig(b,phase4,k8,[]).
checkifzulaessig(b,phase6,fa1,[]).
checkifzulaessig(b,phase6,k6,[]).
checkifzulaessig(b,phase6,k8,[]).
checkifzulaessig(b,phase6,b1,[]).
%zulässig
checkifzulaessig(b,_,Ausloeser,GG):-ausloeser(b,Ausloeser,GG).



