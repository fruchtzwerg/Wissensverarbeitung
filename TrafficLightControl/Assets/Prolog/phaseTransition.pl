/**
* Autoren: Tilo Zuelske, Hannes Boers, Laurens Gross und Benjamin Montazer
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Zulaessige und nicht Zulaessige Phasenuebergaenge            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('phaseTransition', [checkifzulaessig/4]).
:- use_module(facts).

%Kreuzung A
%nicht zulaessige Uebergaenge
checkifzulaessig(a, phase14, k10, _) :- !, false.
%zulaessig
checkifzulaessig(a, _, Ausloeser, GG) :- facts:ausloeser(a, Ausloeser, GG),!.

%Kreuzung B
%nicht zulaessige Uebergaenge
checkifzulaessig(b, phase2, keineAktion, _):-!, false.
checkifzulaessig(b, phase3, keineAktion, _):-!, false.
checkifzulaessig(b, phase3, fa1, _):-!, false.
checkifzulaessig(b, phase3, k8, _):-!, false.
checkifzulaessig(b, phase3, k6, _):-!, false.
checkifzulaessig(b, phase2, fa4, _):-!, false.
checkifzulaessig(b, phase5, fa1, _):-!, false.
checkifzulaessig(b, phase5, k8, _):-!, false.
checkifzulaessig(b, phase5, k6, _):-!, false.
checkifzulaessig(b, phase5, k4, _):-!, false.
checkifzulaessig(b, phase4, b1, _):-!, false.
checkifzulaessig(b, phase6, k4, _):-!, false.
checkifzulaessig(b, phase3, boomgate, _):-!, false.
checkifzulaessig(b, phase2, boomgate, _):-!, false.
checkifzulaessig(b, phase5, boomgate, _):-!, false.
checkifzulaessig(b, phase4, fa4, _):-!, false.
checkifzulaessig(b, phase4, fa1, _):-!, false.
checkifzulaessig(b, phase4, k6, _):-!, false.
checkifzulaessig(b, phase4, k8, _):-!, false.
checkifzulaessig(b, phase6, fa1, _):-!, false.
checkifzulaessig(b, phase6, k6, _):-!, false.
checkifzulaessig(b, phase6, k8, _):-!, false.
checkifzulaessig(b, phase6, b1, _):-!, false.
%zulaessig
checkifzulaessig(b, _, Ausloeser, GG) :- facts:ausloeser(b, Ausloeser, GG),!.
