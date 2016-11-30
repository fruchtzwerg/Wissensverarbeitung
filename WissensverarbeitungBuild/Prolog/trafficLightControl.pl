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
:- use_module(sequenceTransition).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Attribute zur Ueberwachung                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Standardsequence fuer die Kreuzungen A und B
defaultSequence(a, sequence11).
defaultSequence(b, sequence1).

%zwei queues fuer die Berechnung der naechsten Ampelsquence
queue(a,[]).
queue(b,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRAEDIKAT    neues Event an Queue anfuegen(von C# (:heart:)aufrufbar)%
% Trigger darf noch nicht in der Queue enthalten sein sonst false               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event(Junction,Trigger,Sequence):-
                             queue(Junction,Queue),
                             not(member([Trigger,_],Queue)),
                             checkTriggerIsValid(Junction,Trigger,IsValidTrigger,Sequence),
                             append(Queue,[[Trigger,IsValidTrigger]],QueueNew),
                             retract(queue(Junction,_)),
                             assert(queue(Junction,QueueNew)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   neues Event auf Zulaessigkeit pruefen                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%IsValidTrigger wird 1 wenn zulaessig
%IsValidTrigger wird 0 wenn NICHT zulaessig
checkTriggerIsValid(Junction,Trigger,IsValidTrigger,Sequence):-
                                              sequenceTransition:validTrigger(Junction,Sequence,Trigger,Result) -> IsValidTrigger = 1
                                              ;
                                              IsValidTrigger = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HAUPTPRAEDIKAT    Anfordern der naechsten Sequence(von C# aufrufbar)            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nextSequence(Junction,GreenLights,Sequence):-
                               queue(Junction, Queue),
                               nextSequenceFromQueue(Queue, Junction, Sequence, NextSequence),
                               SequenceX=..[NextSequence,Junction,GreenLights],
                               call(SequenceX),
                               !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pruefen ob die Queue leer ist                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%der Uebergang bei Junction b von sequence2 bzw sequence3 zur sequence1 ist nicht zulaessig
%laut Spetzifikation muss auf sequence2/3 entweder sequence4 oder 5 folgen.
%da der Standard keine weiteren Angaben macht, haben wir uns entschieden
%sequence4 zu triggern, sofern kein gegensätzliches Ereignis (b1) aufgetreten ist,
%da bei sequence4 mehr Ampeln auf grün stehen.
nextSequenceFromQueue([],b, Sequence, NextSequence):-
                               (sequence2 == Sequence ; sequence3 == Sequence),
                               NextSequence=sequence4,
                               !.

nextSequenceFromQueue([], Junction, _, NextSequence):-defaultSequence(Junction,NextSequence),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Pruefung des Queue Kopfes:                                                    %
%bei 0 -> Trigger an Queue vorne entnehmen und hinten wieder anhaengen  %
%bei 1 -> naechster Zustand und Trigger fuer den Zustand aus Q entfernen %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Queue = [[Trigger,IsValid]|Tail]
nextSequenceFromQueue(Queue, Junction, Sequence, NextSequence):-
                                    forall(member([_,IsValid],Queue), 0==IsValid),
                                    nextSequenceFromQueue([],Junction,Sequence,NextSequence),
                                    validateAndRemoveDups(Junction,NextSequence,Queue, []).

nextSequenceFromQueue([Head|Tail], Junction, _, NextSequence):-
                                   [Trigger,IsValid] = Head,
                                   1==IsValid,
                                   facts:trigger(Junction,Trigger,[_,NextSequence|_]),
                                   validateAndRemoveDups(Junction,NextSequence,Tail, []),
                                   !.

nextSequenceFromQueue([Head|Tail], Junction, _, _):-
                                  append(Tail,[Head],QueueNew),
                                  nextSequenceFromQueue(QueueNew, Junction, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pruefen ob Trigger in der Queue zur gleichenSequenceaenderung fuehren      %
% ,wie der bereits akzeptierte                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%validateAndRemoveDups(Junction, neuePhase, aktuelleQueue, TmpList)
validateAndRemoveDups(Junction, _, [], TmpList):-
                                    retract(queue(Junction, _)),
                                    assert(queue(Junction, TmpList)).

validateAndRemoveDups(Junction,NextSequence,[Head|Tail], TmpList):-
                                    [Trigger|_]=Head,
                                    facts:trigger(Junction,Trigger,[_,Sequence|_]),
                                    not(NextSequence=Sequence) ->
                                    (
                                        checkTriggerIsValid(Junction, Trigger, IsValid, NextSequence),
                                        append(TmpList,[[Trigger,IsValid]],TmpListNew),
                                        validateAndRemoveDups(Junction,NextSequence,Tail,TmpListNew)
                                    )
                                    ;
                                    validateAndRemoveDups(Junction,NextSequence,Tail,TmpList).
