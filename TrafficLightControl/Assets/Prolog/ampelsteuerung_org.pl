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


:- use_module(facts).
:- use_module(phaseTransition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Attribute zur Überwachung                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Überwachung der momentanen Phase für beide Ampelanlagen ,Anfangszustand
momentanePhase(a, phase11).
momentanePhase(b, phase1).

standardPhase(a, phase11).
standardPhase(b, phase1).

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
% HAUPTPRÄDIKAT     neues Ereignis an Queue anfügen(von C# aufrufbar)          %
% Ausloeser darf noch nicht in der Queue enthalten sein -> false               %
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
%                   neues Ereignis auf Zulässigkeit prüfen                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAusloeser(Kreuzung,Ausloeser,Wahrheitswert):-
                                              momentanePhase(Kreuzung,Phase)
                                              ,
                                              phaseTransition:checkifzulaessig(Kreuzung,Phase,Ausloeser,Ergebnis)
                                              ,
                                              zulaessig(Ergebnis,Wahrheitswert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRÄDIKAT    Anfordern der nächsten Phase(von C# aufrufbar)              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Fall 1 die Queue ist leer
getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
                               ,
                               checkQueueIsEmpty(Kreuzung, Q, Phaseneu)
                               ,
                               retract(momentanePhase(Kreuzung,_))
                               ,
                               assert(momentanePhase(Kreuzung, Phaseneu))
                               ,
                               Y=..[Phaseneu, Kreuzung, Gruenegesamt]
                               ,
                               call(Y)
                               ,
                               !.

%Fall 2 die Queue enthält nur Falsche Elemente
getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung,Q)
                               ,
                               checkQueueComplete(Q, Kreuzung, Phaseneu)
                               ,
                               retract(momentanePhase(Kreuzung,_))
                               ,
                               assert(momentanePhase(Kreuzung, Phaseneu))
                               ,
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt]
                               ,
                               call(Y)
                               ,
                               reCheckAusloeserInQueue(Kreuzung, Q)
                               ,!.

%Fall 3 es ist mindestens wahres Element enthalten in der Queue
%Normale Abarbeitung der Queue
getnextPhase(Kreuzung,Gruenegesamt):-
                               queue(Kreuzung, Q)
                               ,
                               doCheck(Q,Kreuzung, Phaseneu)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pruefen ob die Queue leer ist                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%der Uebergang bei Kreuzung b von phase2 bzw phase3 zur phase1 ist nicht zulaessig
checkQueueIsEmpty(b,[],Phaseneu):-
                               momentanePhase(b,MomPhase)
                               ,
                               (phase2 = MomPhase ; phase3 = MomPhase)
                               ,
                               Phaseneu=MomPhase
                               ,
                               !.

checkQueueIsEmpty(Kreuzung,[],Phaseneu):-standardPhase(Kreuzung,Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pruefen ob die Queue ueberhaupt wahre Elemente(Ausloeser) enthält            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%bei Kreuzung B gibt es zwei Phasen, in denen man nicht in die Grundphase kommt,
% in diesem Fall bleibe in der aktuellen Phase
checkQueueComplete([],b,Phaseneu):-
                               momentanePhase(b, MomPhase)
                               ,
                               (phase2 = MomPhase ; phase3 = MomPhase)
                               ,
                               Phaseneu=MomPhase
                               ,
                               !.
%Wenn nur falsche Werte in der Queue sind gehe zur Grundphase zurueck
checkQueueComplete([], b, phase1):-!.
checkQueueComplete([], a, phase11):-!.

%checkt ob alle Werte in der Queue falsch sind
checkQueueComplete([[_, Zulaessig]|T], Kreuzung, Phaseneu):-
                                         falsch=Zulaessig
                                         ,
                                         checkQueueComplete(T, Kreuzung, Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Wiederholtes pruefen der Queue,durch Entnahme des Kopfes         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Abbruchbedingung für "While Schleife", bei erfolgreicher Entnahme eines Ausloesers
%wird die Queue von der Laenge her kleiner -> Abbruch der Schleife
doCheck(Q,Kreuzung,_):-
                    length(Q,NeueLaenge)
                    ,
                    laengeAlt(Kreuzung,AlteLaenge)
                    ,
                    NeueLaenge<AlteLaenge,!.

%Vor Pruefung des QueueKopfes Queue Laenge feststellen und laengeAlt setzen
%Dann immer wieder den QueueKopf entnehmen und prüfen
%wenn falsch Suche nach wahrem Ausloeser fortsetzen->erneut aufrufen
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Prüfung des Queue Kopfes:                                                     %
%bei falsch -> Ausloeser an Queue vorne entnehmen und hinten wieder anhaengen  %
%bei richig -> naechster Zustand und Ausloser fuer den Zustand aus Q entfernen %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkQueueHead([[Ausloeser,Zulaessig]|T],Kreuzung,Phaseneu):-
                                   wahr=Zulaessig
                                   ,
                                   facts:ausloeser(Kreuzung,Ausloeser,[_,Phaseneu|_])
                                   ,
                                   retract(queue(Kreuzung,_))
                                   ,
                                   assert(queue(Kreuzung,T))
                                   ,
                                   checkAlreadyAccepted(Kreuzung,Phaseneu,T)
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
                                    checkAusloeser(Kreuzung, Ausloeser, Zulaessig)
                                    ,
                                    queuetemp(Kreuzung, TQ)
                                    ,
                                    append(TQ, [[Ausloeser, Zulaessig]], L)
                                    ,
                                    retract(queuetemp(Kreuzung, _))
                                    ,
                                    assert(queuetemp(Kreuzung, L))
                                    ,
                                    reCheckAusloeserInQueue(Kreuzung, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prüfen ob Auslöeser in der Queue zur gleichen Phasenänderung führen          %
% ,wie der bereits akzeptierte                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAlreadyAccepted(Kreuzung, _, []):-
                                    queuetemp(Kreuzung, X)
                                    ,
                                    retract(queue(Kreuzung, _))
                                    ,
                                    assert(queue(Kreuzung, X))
                                    ,
                                    retract(queuetemp(_, _))
                                    ,
                                    assert(queuetemp(_,[])).

checkAlreadyAccepted(Kreuzung,Phaseneu,[[Ausloeser,_]|T]):-
                                            facts:ausloeser(Kreuzung, Ausloeser, [_, Phase|_])
                                            ,
                                            Phaseneu=Phase
                                            ,
                                            checkAlreadyAccepted(Kreuzung, Phaseneu, T).

checkAlreadyAccepted(Kreuzung,Phaseneu,[H|T]):-
                                            [Ausloeser|_]=H
                                            ,
                                            facts:ausloeser(Kreuzung,Ausloeser,[_,Phase|_])
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
