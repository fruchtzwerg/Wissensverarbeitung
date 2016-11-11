% Autoren: Tilo Zuelske und Hannes Boers
% Datum: 27.10.2016

%Configuration der Wissensbank
:- style_check(-discontiguous).
:- dynamic queue/2.
:- dynamic momentanePhase/2.
:- dynamic laengeAlt/2.
:- dynamic queuetemp/2.

:-set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).
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
ausloeser(a,b3,GG):- phase12(a,GG).
ausloeser(a,k10,GG):-phase13(a,GG).
ausloeser(a,fa10,GG):- phase14(a,GG).
ausloeser(a,k12,GG):- phase14(a,GG).
ausloeser(a,fa11,_):-!,
                     write('use fa10 instead'),
                     fail.


%Phasen Ampelkreuzung B
phase1(b,[[h3,fg8,fg6,k7,b2,k1,k5,k14,k2,k3,fg3,h1],phase1,35]).
phase2(b,[[fa7,k8,fg6,fa1,fa2,k6,k5],phase2,10]).
phase3(b,[[fa7,fg5,fg8,fa1,fa2,h4,k6,h2,fa5,fa4],phase3,9]).
phase4(b,[[h3,fg8,k4,k7,b2,k1,k2],phase4,5]).
phase5(b,[[k4,k7,b1,k2,b2],phase5,5]).
phase6(b,[[h3,fg8,fg5,k7,b2,k1,k3,k2,fg3,h1],phase6,15]).

%Regeln Kreuzung B
ausloeser(b,fa4,GG):-phase3(b,GG).
ausloeser(b,fa1,GG):-phase2(b,GG).
ausloeser(b,k8,GG):-phase2(b,GG).
ausloeser(b,k6,GG):-phase2(b,GG).
ausloeser(b,k4,GG):-phase4(b,GG).
ausloeser(b,b1,GG):-phase5(b,GG).
ausloeser(b,schranke,GG):-phase6(b,GG).

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
checkifzulaessig(b,phase3,fa1,[]):-!.
checkifzulaessig(b,phase3,k8,[]):-!.
checkifzulaessig(b,phase3,k6,[]):-!.
checkifzulaessig(b,phase2,fa4,[]):-!.
checkifzulaessig(b,phase5,fa1,[]):-!.
checkifzulaessig(b,phase5,k8,[]):-!.
checkifzulaessig(b,phase5,k6,[]):-!.
checkifzulaessig(b,phase5,k4,[]):-!.
checkifzulaessig(b,phase4,b1,[]):-!.
checkifzulaessig(b,phase6,k4,[]):-!.
checkifzulaessig(b,phase3,schranke,[]):-!.
checkifzulaessig(b,phase2,schranke,[]):-!.
checkifzulaessig(b,phase5,schranke,[]):-!.
checkifzulaessig(b,phase4,fa4,[]):-!.
checkifzulaessig(b,phase4,fa1,[]):-!.
checkifzulaessig(b,phase4,k6,[]):-!.
checkifzulaessig(b,phase4,k8,[]):-!.
checkifzulaessig(b,phase6,fa1,[]):-!.
checkifzulaessig(b,phase6,k6,[]):-!.
checkifzulaessig(b,phase6,k8,[]):-!.
checkifzulaessig(b,phase6,b1,[]):-!.
%zulässig
checkifzulaessig(b,_,Ausloeser,GG):-ausloeser(b,Ausloeser,GG),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Attribute zur Überwachung                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Überwachung der momentanen Phase für beide Ampelanlagen ,Anfangszustand
momentanePhase(a,phase11).
momentanePhase(b,phase1).

standardPhase(a,phase11).
standardPhase(b,phase1).

%zwei queues für die Berechnung der nächsten Ampelphase
queue(a,[]).
queue(b,[]).
queuetemp(_,[]).

%Wahrheitswert
zulaessig([],falsch).
zulaessig(_,wahr).

%Alte Laengen der Queues zur Hilfename für die "while-Schleife"
laengeAlt(a,0).
laengeAlt(b,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Ereignis an Queue anfügen                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neuesEreignis(Ampelkreuzung,Ausloeser):-
                             queue(Ampelkreuzung,Q)
                             ,
                             not(member([Ausloeser,_],Q))
                             ,
                             checkAusloeser(Ampelkreuzung,Ausloeser,W)
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
                                              zulaessig(Ergebnis,Wahrheitswert).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prüfen ob Auslöeser in der Queue zur gleichen Phasenänderung führen          %
% ,wie der bereits akzeptierte                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAlreadyAccepted(Kreuzung,_,[]):-
                                   queuetemp(Kreuzung,X)
                                   ,
                                   retract(queue(Kreuzung,_))
                                   ,
                                   assert(queue(Kreuzung,X))
                                   ,
                                   retract(queuetemp(_,_))
                                   ,
                                   assert(queuetemp(_,[])).

checkAlreadyAccepted(Kreuzung,Phaseneu,[[Ausloeser,_]|T]):-
                                           ausloeser(Kreuzung,Ausloeser,[_,Phase|_])
                                           ,
                                           Phaseneu=Phase
                                           ,
                                           checkAlreadyAccepted(Kreuzung,Phaseneu,T).

checkAlreadyAccepted(Kreuzung,Phaseneu,[H|T]):-
                                           [Ausloeser|_]=H
                                           ,
                                           ausloeser(Kreuzung,Ausloeser,[_,Phase|_])
                                           ,
                                           not(Phaseneu=Phase)
                                           ,
                                           queuetemp(Kreuzung,X)
                                           ,
                                           append(X,[H],L)
                                           ,
                                           retract(queuetemp(_,_))
                                           ,
                                           assert(queuetemp(Kreuzung,L))
                                           ,
                                           checkAlreadyAccepted(Kreuzung,Phaseneu,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  alle Auslöser(bereits in der Queue)bei Phasenänderung erneut überprüfen     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reCheckAusloeserInQueue(Kreuzung,[]):-
                                   queuetemp(Kreuzung,X)
                                   ,
                                   retract(queue(Kreuzung,_))
                                   ,
                                   assert(queue(Kreuzung,X))
                                   ,
                                   retract(queuetemp(_,_))
                                   ,
                                   assert(queuetemp(_,[])).

reCheckAusloeserInQueue(Kreuzung,[[Ausloeser,_]|T]):-
                                   checkAusloeser(Kreuzung,Ausloeser,Zulaessig)
                                   ,
                                   queuetemp(Kreuzung,TQ)
                                   ,
                                   append(TQ,[[Ausloeser,Zulaessig]],L)
                                   ,
                                   retract(queuetemp(Kreuzung,_))
                                   ,
                                   assert(queuetemp(Kreuzung,L))
                                   ,
                                   reCheckAusloeserInQueue(Kreuzung,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Anfordern der nächsten Phase                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
                               ,
                               checkQueueIsEmpty(Kreuzung,Q,Phaseneu)
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

getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
                               ,
                               checkQueueComplete(Q,Kreuzung,Phaseneu)
                               ,
                               retract(momentanePhase(Kreuzung,_))
                               ,
                               assert(momentanePhase(Kreuzung,Phaseneu))
                               ,
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt]
                               ,
                               call(Y)
                               ,
                               reCheckAusloeserInQueue(Kreuzung,Q)
                               ,!.

getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
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
                               queue(Kreuzung,Qneu)
                               ,
                               reCheckAusloeserInQueue(Kreuzung,Qneu)
                               ,
                               length(Qneu,QneuLaenge)
                               ,
                               retract(laengeAlt(Kreuzung,_))
                               ,
                               assert(laengeAlt(Kreuzung,QneuLaenge))
                               ,
                               !.
%der Übergang bei Kreuzung b von phase2 bzw phase3 zur phase1 ist nicht zulässig
checkQueueIsEmpty(b,[],Phaseneu):-
                               momentanePhase(b,MomPhase)
                               ,
                               (phase2=MomPhase;phase3=MomPhase)
                               ,
                               Phaseneu=MomPhase
                               ,
                               !.
                               
checkQueueIsEmpty(Kreuzung,[],Phaseneu):-standardPhase(Kreuzung,Phaseneu).


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
checkQueueHead([[Ausloeser,Zulaessig]|T],Kreuzung,PhaseNeu):-
                                   wahr=Zulaessig
                                   ,
                                   ausloeser(Kreuzung,Ausloeser,[_,PhaseNeu|_])
                                   ,
                                   retract(queue(Kreuzung,_))
                                   ,
                                   assert(queue(Kreuzung,T))
                                   ,
                                   checkAlreadyAccepted(Kreuzung,PhaseNeu,T)
                                   ,
                                   !.

checkQueueHead([H|T],Kreuzung,_):-
                                  [_,Zulaessig|_]=H
                                  ,
                                  falsch=Zulaessig
                                  ,
                                  append(T,[H],Q)
                                  ,
                                  retract(queue(Kreuzung,_))
                                  ,
                                  assert(queue(Kreuzung,Q)).


%checken ob die Q überhaupt wahre Elemente(Anforderungen) enthält
checkQueueComplete([],b,Phaseneu):-
                               momentanePhase(b,MomPhase)
                               ,
                               (phase2=MomPhase;phase3=MomPhase)
                               ,
                               Phaseneu=MomPhase
                               ,
                               !.

checkQueueComplete([],b,phase1):-!.
checkQueueComplete([],a,phase11):-!.
checkQueueComplete([[_,Zulaessig]|T],Kreuzung,PhaseNeu):-
                                         falsch=Zulaessig
                                         ,
                                         checkQueueComplete(T,Kreuzung,PhaseNeu).



