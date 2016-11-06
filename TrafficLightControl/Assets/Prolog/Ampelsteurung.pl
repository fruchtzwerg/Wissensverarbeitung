% Autoren: Tilo Zuelske und Hannes Boers
% Datum: 27.10.2016

%Configuration der Wissensbank
:- style_check(-discontiguous).
:- dynamic queue/2.
:- dynamic momentanePhase/2.
:- dynamic laengeAlt/2.
:-set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Attribute zur Überwachung                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Überwachung der momentanen Phase für beide Ampelanlagen ,Anfangszustand
momentanePhase(a,phase11).
momentanePhase(b,phase1).

%zwei queues für die Berechnung der nächsten Ampelphase
queue(a,[]).
queue(b,[]).

%Wahrheitswert
bool([],falsch).
bool([H|T],wahr).

%Alte Laengen der Queues zur Hilfename für die "while-Schleife"
laengeAlt(a,0).
laengeAlt(b,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Ereignis an Queue anfügen                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neuesEreignis(Ampelkreuzung,Ausloeser):-
                             checkAusloeser(Ampelkreuzung,Ausloeser,W)
                             ,
                             queue(Ampelkreuzung,Q)
                             ,
                             append(Q,[[Ausloeser,W]],L)
                             ,
                             retract(queue(Ampelkreuzung,_))
                             ,
                             assert(queue(Ampelkreuzung,L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Ereignis auf Zulässigkeit prügen                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAusloeser(Kreuzung,Ausloeser,Wahrheitswert):-
                                              momentanePhase(Kreuzung,Phase)
                                              ,
                                              checkifzulaessig(Kreuzung,Phase,Ausloeser,Ergebnis)
                                              ,
                                              bool(Ergebnis,Wahrheitswert).


                             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Definition der Ampelanlagen                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ampel Logik
% Gültige Zustände der Kreuzung nach STVO, für jede AmpelAnlage definierbar

%Phasen Ampelkreuzung A
%Aus Grunde dem heraus, dass die Originalen Phasen mit einer zusätzlichen 1 zur Unterscheidung notiert wurden,
%übernehmen auch wir diese Konvention
phase11(a,[[k9,k13,b4,h6,fg9],phase11,35]).
phase12(a,[[b3,k9,fg9],phase12,10]).
phase13(a,[[h5,fa11,fa10,k11,k10],phase13,20]).
phase14(a,[[h5,fa11,fa10,k12],phase14,18]).

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
%phase1(b,[h3,fg8,fg6,k7,b2,k1,k5,k14,k2,k3,fg3,h1,phase1]).
%phase2(b,[gruen(fa7),gruen(k8),gruen(fg6),gruen(fa1),gruen(fa2),gruen(k6),gruen(k5),phase2]).
%phase3(b,[gruen(fa7),gruen(fg5),gruen(fg8),gruen(fa1),gruen(fa2),gruen(h4),gruen(k6),gruen(h2),gruen(fa5),gruen(fa4),phase3]).
%phase4(b,[gruen(h3),gruen(fg8),gruen(k4),gruen(k7),gruen(b2),gruen(k1),gruen(k2),phase4]).
%phase5(b,[gruen(k4),gruen(k7),gruen(b1),gruen(k2),gruen(b2),phase5]).
%phase6(b,[gruen(h3),gruen(fg8),gruen(fg5),gruen(k7),gruen(b2),gruen(k1),gruen(k3),gruen(k2),gruen(fg3),gruen(h1),phase6]).

%Regeln Kreuzung B
%ausloeser(b,keineAktion,GG):-phase1(b,GG).
%ausloeser(b,fa4,GG):-phase3(b,GG).
%ausloeser(b,fa1,GG):-phase2(b,GG).
%ausloeser(b,k8,GG):-phase2(b,GG).
%ausloeser(b,k6,GG):-phase2(b,GG).
%ausloeser(b,k4,GG):-phase4(b,GG).
%ausloeser(b,b1,GG):-phase5(b,GG).
%ausloeser(b,schranke,GG):-phase6(b,GG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Anfordern der nächsten Phase                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
                               ,
                               checkQueueComplete(Q,Kreuzung,Phaseneu)
                               ,
                               doCheck(Q,Kreuzung,Phaseneu)
                               ,
                               retract(momentanePhase(Kreuzung,_))
                               ,
                               assert(momentanePhase(Kreuzung,Phaseneu))
                               ,
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt]
                               ,
                               call(Y)
                               ,
                               !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Queue Operationen                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Abbruchbedingung für "While Schleife"
doCheck(Q,Kreuzung,_):-
                    length(Q,NeueLaenge)
                    ,
                    laengeAlt(Kreuzung,AlteLaenge)
                    ,
                    NeueLaenge<AlteLaenge,!.

doCheck(Q,Kreuzung,Phaseneu):-
                           length(Q,X)
                           ,
                           retract(laengeAlt(Kreuzung,_))
                           ,
                           assert(laengeAlt(Kreuzung,X))
                           ,
                           checkQueueHead(Q,Kreuzung,Phaseneu)
                           ,
                           queue(Kreuzung,Qneu)
                           ,
                           doCheck(Qneu,Kreuzung,Phaseneu).

%Prüfung des Queue Kopfes bei falsch Element(Anforderungen) an Queue vorne entnehmen und
%hinten wieder anhängen
%bei richig ->nächster Zustand und Element(Anforderungen) aus Q entfernen
checkQueueHead([H|T],Kreuzung,PhaseNeu):-
                                   [Ausloeser,R|Rest]=H
                                   ,
                                   wahr=R
                                   ,
                                   ausloeser(Kreuzung,Ausloeser,[Liste,PhaseNeu|Y])
                                   ,
                                   retract(queue(Kreuzung,_))
                                   ,
                                   assert(queue(Kreuzung,T)),!.

checkQueueHead([H|T],Kreuzung,PhaseNeu):-
                                  [K,R|Rest]=H
                                  ,
                                  falsch=R
                                  ,
                                  append(T,[H],Q)
                                  ,
                                  retract(queue(Kreuzung,_))
                                  ,
                                  assert(queue(Kreuzung,Q)).


%checken ob die Q überhaupt wahre Elemente(Anforderungen) enthält
checkQueueComplete([],b,phase1):-!.
checkQueueComplete([],a,phase11):-!.
checkQueueComplete([H|T],Kreuzung,PhaseNeu):-
                                         [K,R|Rest]=H
                                         ,
                                         wahr=R,!.

checkQueueComplete([H|T],Kreuzung,PhaseNeu):-
                                         [K,R|Rest]=H
                                         ,
                                         falsch=R
                                         ,!,
                                         checkQueueComplete(T,Kreuzung,PhaseNeu)
                                         .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Zulässige und nicht Zulässige Phasenübergänge                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Kreuzung A
%nicht zulässige Übergänge
checkifzulaessig(a,phase14,k10,[]):-!.
%zulässig
checkifzulaessig(a,_,Ausloeser,GG):-ausloeser(a,Ausloeser,GG),!.

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
checkifzulaessig(b,_,Ausloeser,GG):-ausloeser(b,Ausloeser,GG),!.



