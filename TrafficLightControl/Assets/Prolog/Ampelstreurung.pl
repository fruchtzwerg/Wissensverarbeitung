% Autoren: Tilo Zuelske und Hannes Boers
% Datum: 27.10.2016

%Ampel Logik
% Gültige Zustände der Kreuzung nach STVO, für jede AmpelAnlage definierbar
zustand1(a,[rot(ampel2),rot(ampel3)],[gruen(ampel1),gruen(ampel4)]).
zustand2(a,[rot(ampel1),rot(ampel4)],[gruen(ampel2),gruen(ampel3)]).

zustand1(b,[rot(ampel5),rot(ampel6)],[gruen(ampel7),gruen(ampel8)]).
zustand2(b,[rot(ampel7),rot(ampel8)],[gruen(ampel5),gruen(ampel6)]).

zustand1(c,[rot(ampel9),rot(ampel10)],[gruen(ampel11),gruen(ampel12)]).
zustand2(c,[rot(ampel11),rot(ampel12)],[gruen(ampel9),gruen(ampel10)]).

%Anlagen und ihre Zustände
%Anlage A
gib_zustandAnlage(a,zustand1,Rot,Gruen):-zustand1(a,Rot,Gruen).
gib_zustandAnlage(a,zustand2,Rot,Gruen):-zustand2(a,Rot,Gruen).

%Anlage B
gib_zustandAnlage(b,zustand1,Rot,Gruen):-zustand1(b,Rot,Gruen).
gib_zustandAnlage(b,zustand2,Rot,Gruen):-zustand2(b,Rot,Gruen).

%Anlage C
gib_zustandAnlage(c,zustand1,Rot,Gruen):-zustand1(c,Rot,Gruen).
gib_zustandAnlage(c,zustand2,Rot,Gruen):-zustand2(c,Rot,Gruen).

%zu einem Weltzustand gehören mehrere Ampelanlagen und deren Zustände
weltzustand1([a(zustand1),b(zustand1),c(zustand1)]).
weltzustand2([a(zustand2),b(zustand2),c(zustand2)]).


%übergebe derzeitigen Weltzustand und erhalte Grüne und Rote Ampeln des nächsten Zustandes
gib_Weltzustand(weltzustand1,Rotgesamt,Gruengesamt):- weltzustand2(ListeDerKreuzungen)
                                                      ,
                                                      gib_Zustand_Kreuzung(ListeDerKreuzungen,Rotgesamt,Gruengesamt).
gib_Weltzustand(weltzustand2,Rotgesamt,Gruengesamt):- weltzustand1(ListeDerKreuzungen)
                                                      ,
                                                      gib_Zustand_Kreuzung(ListeDerKreuzungen,Rotgesamt,Gruengesamt).

%Abbruchbedingung
gib_Zustand_Kreuzung([],[],[]).
gib_Zustand_Kreuzung([H|T],RG,GG):- H=..[Kreuzung,Zustand]
                                    ,
                                    gib_zustandAnlage(Kreuzung,Zustand,ZR,ZG)
                                    ,
                                    gib_Zustand_Kreuzung(T,R,G)
                                    ,
                                    append(ZR,R,RG)
                                    ,
                                    append(ZG,G,GG).
