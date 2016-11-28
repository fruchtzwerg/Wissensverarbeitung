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
%                         Attribute zur Ueberwachung                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

standardPhase(a, phase11).
standardPhase(b, phase1).

%zwei queues fuer die Berechnung der nächsten Ampelphase
queue(a,[]).
queue(b,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRAEDIKAT    neues Ereignis an Queue anfuegen(von C# (:heart:)aufrufbar)%
% Ausloeser darf noch nicht in der Queue enthalten sein -> false               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%neuesEreignis(Kreuzung,Ausloeser,aktuellePhase)
neuesEreignis(Junction,Trigger,Phase):-
                             queue(Junction,Queue),
                             not(member([Trigger,_],Queue)),
                             checkAusloeser(Junction,Trigger,W,Phase),
                             append(Queue,[[Trigger,W]],L),
                             retract(queue(Junction,_)),
                             assert(queue(Junction,L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Ereignis auf Zulaessigkeit pruefen                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Wahrheitswert wird 1 wenn zulaessig
%Wahrheitswert wird 0 wenn NICHT zulaessig
%checkAusloeser(Kreuzung,Ausloeser,wahrheitswert,aktuellePhase)
checkAusloeser(Kreuzung,Trigger,Wahrheitswert,Phase):-
                                              phaseTransition:checkifzulaessig(Kreuzung,Phase,Trigger,Ergebnis) -> Wahrheitswert = 1
                                              ;
                                              Wahrheitswert = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRAEDIKAT    Anfordern der naechsten Phase(von C# aufrufbar)             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%getnextPhase(Kreuzung, Gruenegesamt, aktuellePhase)
%Fall 1 die Queue enthaelt nur Falsche Elemente
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
                               doCheck(Queue, Kreuzung, Phaseneu),
                               Y=..[Phaseneu,Kreuzung,Gruenegesamt],
                               call(Y),
                               !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pruefen ob die Queue leer ist                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%der Uebergang bei Kreuzung b von phase2 bzw phase3 zur phase1 ist nicht zulaessig
%laut Spetzifikation muss auf phase2/3 entweder phase4 oder 5 folgen.
%da der Standard keine weiteren Angaben macht, haben wir uns entschieden
%phase4 zu triggern, sofern kein gegensätzliches Ereignis (b1) aufgetreten ist,
%da bei phase4 mehr Ampeln auf grün stehen.
%checkQueueComplete([],b, aktuellePhase, neuePhase)
checkQueueComplete([],b, Phase, Phaseneu):-
                               (phase2 == Phase ; phase3 == Phase),
                               Phaseneu=phase4,
                               !.

checkQueueComplete([], Kreuzung, _, Phaseneu):-standardPhase(Kreuzung,Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pruefen ob die Queue ueberhaupt wahre Elemente(Ausloeser) enthält            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%checkt ob alle Werte in der Queue falsch sind
%checkQueueComplete([[Kreuzung, Zulaessig]|T], Kreuzung, aktuellePhase, neuePhase)
checkQueueComplete([[_, Zulaessig]|T], Kreuzung, Phase, Phaseneu):-
                                         0==Zulaessig,
                                         checkQueueComplete(T, Kreuzung, Phase, Phaseneu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Wiederholtes pruefen der Queue,durch Entnahme des Kopfes        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Vor Pruefung des QueueKopfes Queue Laenge feststellen und laengeAlt setzen
%Dann immer wieder den QueueKopf entnehmen und prüfen
%wenn falsch Suche nach wahrem Ausloeser fortsetzen->erneut aufrufen
%var = true wenn die variable frei ist
%doCheck(aktuelleQueue,Kreuzung,neuePhase)
doCheck(Queue,Kreuzung,Phaseneu):-
                           checkQueueHead(Queue,Kreuzung,Phaseneu, Qneu),  
                           var(Phaseneu),
                           doCheck(Qneu,Kreuzung,Phaseneu)
                           ;
                           checkQueueHead(Queue,Kreuzung,Phaseneu, Qneu),
                           !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Pruefung des Queue Kopfes:                                                    %
%bei falsch -> Ausloeser an Queue vorne entnehmen und hinten wieder anhaengen  %
%bei richig -> naechster Zustand und Ausloser fuer den Zustand aus Q entfernen %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checkQueueHead([Head|T], Kreuzung, neuePhase, aktuelleQueue)
checkQueueHead([Head|T], Kreuzung, Phaseneu, _):-
                                   [Trigger,Zulaessig] = Head,
                                   1==Zulaessig,
                                   facts:ausloeser(Kreuzung,Trigger,[_,Phaseneu|_]),
                                   checkAlreadyAccepted(Kreuzung,Phaseneu,T, []),
                                   !.

checkQueueHead([Head|T], Kreuzung, _, Queue):-
                                  [_,Zulaessig]=Head,
                                  0==Zulaessig,                                   
                                  append(T,[Head],Queue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pruefen ob Ausloeeser in der Queue zur gleichen Phasenaenderung fuehren      %
% ,wie der bereits akzeptierte                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checkAlreadyAccepted(Kreuzung, neuePhase, aktuelleQueue, TmpList)
checkAlreadyAccepted(Kreuzung, _, [], TmpList):-
                                    retract(queue(Kreuzung, _)),
                                    assert(queue(Kreuzung, TmpList)).

checkAlreadyAccepted(Kreuzung,Phaseneu,[H|T], TmpList):-
                                    [Trigger|_]=H,
                                    facts:ausloeser(Kreuzung,Trigger,[_,Phase|_]),
                                    not(Phaseneu=Phase) ->
                                    (
                                        checkAusloeser(Kreuzung, Trigger, Zulaessig, Phaseneu),
                                        append(TmpList,[[Trigger,Zulaessig]],L),
                                        checkAlreadyAccepted(Kreuzung,Phaseneu,T,L)
                                    )
                                    ;
                                    checkAlreadyAccepted(Kreuzung,Phaseneu,T,TmpList).
