/**
* Autoren: Tilo Zuelske, Hannes Boers, Laurens Gross und Benjamin Montazer
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Zulaessige und nicht Zulaessige Phasenuebergaenge            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module('sequenceTransition', [validTrigger/4]).
:- use_module(facts).

%Kreuzung A
%nicht zulaessige Uebergaenge
validTrigger(a, sequence14, k10, _) :- !, false.
%zulaessig
validTrigger(a, _, Trigger, GreenLights) :- facts:trigger(a, Trigger, GreenLights),!.

%Kreuzung B
%nicht zulaessige Uebergaenge
validTrigger(b, sequence2, noAction, _):-!, false.
validTrigger(b, sequence3, noAction, _):-!, false.
validTrigger(b, sequence3, fa1, _):-!, false.
validTrigger(b, sequence3, k8, _):-!, false.
validTrigger(b, sequence3, k6, _):-!, false.
validTrigger(b, sequence2, fa4, _):-!, false.
validTrigger(b, sequence5, fa1, _):-!, false.
validTrigger(b, sequence5, k8, _):-!, false.
validTrigger(b, sequence5, k6, _):-!, false.
validTrigger(b, sequence5, k4, _):-!, false.
validTrigger(b, sequence4, b1, _):-!, false.
validTrigger(b, sequence6, k4, _):-!, false.
validTrigger(b, sequence3, boomgate, _):-!, false.
validTrigger(b, sequence2, boomgate, _):-!, false.
validTrigger(b, sequence5, boomgate, _):-!, false.
validTrigger(b, sequence4, fa4, _):-!, false.
validTrigger(b, sequence4, fa1, _):-!, false.
validTrigger(b, sequence4, k6, _):-!, false.
validTrigger(b, sequence4, k8, _):-!, false.
validTrigger(b, sequence6, fa1, _):-!, false.
validTrigger(b, sequence6, k6, _):-!, false.
validTrigger(b, sequence6, k8, _):-!, false.
validTrigger(b, sequence6, b1, _):-!, false.
%zulaessig
validTrigger(b, _, Trigger, GreenLights) :- facts:trigger(b, Trigger, GreenLights),!.
