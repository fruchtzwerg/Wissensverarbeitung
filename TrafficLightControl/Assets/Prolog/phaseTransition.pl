%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Zulässige und nicht Zulässige Phasenübergänge                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(checkifzulaessig, [checkifzulaessig/4]).
:- use_module(facts).

%Kreuzung A
%nicht zulässige Übergänge
checkifzulaessig(a, facts:phase14, k10, []) :- !.
%zulässig
checkifzulaessig(a, _, Ausloeser, GG) :- facts:ausloeser(a, Ausloeser, GG),!.

%Kreuzung B
%nicht zulässige Übergänge
checkifzulaessig(b, facts:phase3, fa1, []):-!.
checkifzulaessig(b, facts:phase3, k8, []):-!.
checkifzulaessig(b, facts:phase3, k6, []):-!.
checkifzulaessig(b, facts:phase2, fa4, []):-!.
checkifzulaessig(b, facts:phase5, fa1, []):-!.
checkifzulaessig(b, facts:phase5, k8, []):-!.
checkifzulaessig(b, facts:phase5, k6, []):-!.
checkifzulaessig(b, facts:phase5, k4, []):-!.
checkifzulaessig(b, facts:phase4, b1, []):-!.
checkifzulaessig(b, facts:phase6, k4, []):-!.
checkifzulaessig(b, facts:phase3, boomgate, []):-!.
checkifzulaessig(b, facts:phase2, boomgate, []):-!.
checkifzulaessig(b, facts:phase5, boomgate, []):-!.
checkifzulaessig(b, facts:phase4, fa4, []):-!.
checkifzulaessig(b, facts:phase4, fa1, []):-!.
checkifzulaessig(b, facts:phase4, k6, []):-!.
checkifzulaessig(b, facts:phase4, k8, []):-!.
checkifzulaessig(b, facts:phase6, fa1, []):-!.
checkifzulaessig(b, facts:phase6, k6, []):-!.
checkifzulaessig(b, facts:phase6, k8, []):-!.
checkifzulaessig(b, facts:phase6, b1, []):-!.
%zulässig
checkifzulaessig(b, _, Ausloeser, GG) :- facts:ausloeser(b, Ausloeser, GG),!.