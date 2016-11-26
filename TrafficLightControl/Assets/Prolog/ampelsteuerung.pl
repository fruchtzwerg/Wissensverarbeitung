% Autoren: Tilo Zuelske und Hannes Boers und Laurens Gross und Benjamin Montazer
% Datum: 27.10.2016

%Configuration der Wissensbank
:- style_check(-discontiguous).
:- dynamic queue/2.

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

standardPhase(a, phase11).
standardPhase(b, phase1).

%zwei queues für die Berechnung der nächsten Ampelphase
queue(a,[]).
queue(b,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRÄDIKAT     neues Ereignis an Queue anfügen(von C# aufrufbar)          %
% Ausloeser darf noch nicht in der Queue enthalten sein -> false               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neuesEreignis(Junction,Trigger,Phase):-
                             queue(Junction,Queue),
                             not(member([Trigger,_],Queue)),
                             checkAusloeser(Junction,Trigger,W,Phase),
                             append(Queue,[[Trigger,W]],L),
                             retract(queue(Junction,_)),
                             assert(queue(Junction,L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Ereignis auf Zulässigkeit prüfen                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAusloeser(Kreuzung,Trigger,Wahrheitswert,Phase):-
                                              phaseTransition:checkifzulaessig(Kreuzung,Phase,Trigger,Ergebnis) -> Wahrheitswert = 1
                                              ;
                                              Wahrheitswert = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRÄDIKAT    Anfordern der nächsten Phase(von C# aufrufbar)              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Fall 1 die Queue enthält nur Falsche Elemente
getnextPhase(Kreuzung, Gruenegesamt, Phase):-
                               queue(Kreuzung,Queue),
                               checkQueueComplete(Queue, Kreuzung, Phase, Phaseneu),
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt],
                               call(Y),
                               checkAlreadyAccepted(Kreuzung,Phaseneu,Queue,[]),
                            !.

%Fall 2 es ist mindestens ein wahres Element enthalten in der Queue
%Normale Abarbeitung der Queue
getnextPhase(Kreuzung,Gruenegesamt,Phase):-
                               queue(Kreuzung, Queue),
                               length(Queue, QneuLaenge),
                               doCheck(Queue, Kreuzung, Phaseneu, QneuLaenge, QneuLaenge),
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt],
                               call(Y),
                               !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pruefen ob die Queue leer ist                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%der Uebergang bei Kreuzung b von phase2 bzw phase3 zur phase1 ist nicht zulaessig
%laut Spetzifikation muss auf phase2/3 entweder phase4 oder 5 fologen.
%da der Standard keine weiteren Angaben macht, haben wir uns entschieden
%phase4 zu triggern, sofern kein gegensätzliches Ereignis (b1) aufgetreten ist,
%da bei phase4 mehr Ampeln auf grün stehen.
checkQueueComplete([],b, Phase, Phaseneu):-
                               (phase2 = Phase ; phase3 = Phase),
                               Phaseneu=phase4,
                               !.

checkQueueComplete([], Kreuzung, _, Phaseneu):-standardPhase(Kreuzung,Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pruefen ob die Queue ueberhaupt wahre Elemente(Ausloeser) enthält            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%checkt ob alle Werte in der Queue falsch sind
checkQueueComplete([[_, Zulaessig]|T], Kreuzung, Phase, Phaseneu):-
                                         0=Zulaessig,
                                         checkQueueComplete(T, Kreuzung, Phase, Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Wiederholtes pruefen der Queue,durch Entnahme des Kopfes         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Abbruchbedingung für "While Schleife", bei erfolgreicher Entnahme eines Ausloesers
%wird die Queue von der Laenge her kleiner -> Abbruch der Schleife
doCheck(_,_,_, NeueLaenge, AlteLaenge):-
                     NeueLaenge<AlteLaenge,!.

%Vor Pruefung des QueueKopfes Queue Laenge feststellen und laengeAlt setzen
%Dann immer wieder den QueueKopf entnehmen und prüfen
%wenn falsch Suche nach wahrem Ausloeser fortsetzen->erneut aufrufen
doCheck(Q,Kreuzung,Phaseneu,_, AlteLaenge):-
                           checkQueueHead(Q,Kreuzung,Phaseneu, Qneu),
                           length(Qneu,X),
                           doCheck(Qneu,Kreuzung,Phaseneu, X,AlteLaenge).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Prüfung des Queue Kopfes:                                                     %
%bei falsch -> Ausloeser an Queue vorne entnehmen und hinten wieder anhaengen  %
%bei richig -> naechster Zustand und Ausloser fuer den Zustand aus Q entfernen %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkQueueHead([[Trigger, Zulaessig]|T], Kreuzung, Phaseneu, QueueNeu):-
                                   1=Zulaessig,
                                   facts:ausloeser(Kreuzung,Trigger,[_,Phaseneu|_]),
                                   checkAlreadyAccepted(Kreuzung,Phaseneu,T, []),
                                   !.

checkQueueHead([Head|T], Kreuzung, _, Queue):-
                                  [_,Zulaessig|_]=Head,
                                  0=Zulaessig,
                                  append(T,[Head],Queue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prüfen ob Auslöeser in der Queue zur gleichen Phasenänderung führen          %
% ,wie der bereits akzeptierte                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkAlreadyAccepted(Kreuzung, _, [], TmpList):-
                                    retract(queue(Kreuzung, _)),
                                    assert(queue(Kreuzung, TmpList)).

checkAlreadyAccepted(Kreuzung,Phaseneu,[H|T], TmpList):-
                                    [Trigger|_]=H,
                                    checkAusloeser(Kreuzung, Trigger, Zulaessig, Phaseneu),
                                    facts:ausloeser(Kreuzung,Trigger,[_,Phase|_]),
                                    not(Phaseneu=Phase) ->
                                    (
                                        append(TmpList,[[Trigger,Zulaessig]],L),
                                        checkAlreadyAccepted(Kreuzung,Phaseneu,T,L)
                                    )
                                    ;
                                    checkAlreadyAccepted(Kreuzung,Phaseneu,T,TmpList).
