% Autor: %AUTHOR%
% Datum: %DATE%

%Aufgabe 3
% on(c,1)
% clear(2)
% clear(a)
% on(a,b)

%Aufgabe 4
% a) vage
% b) vage
% c) unsicher

%Aufgabe 5

%Aufgabe 9
satz(Numerus, Geschlecht)--> subjekt(Numerus, Geschlecht), praedikat(Numerus).
subjekt(Numerus, Geschlecht) --> artikel(Numerus, Geschlecht), substantiv(Numerus, Geschlecht).
praedikat(Numerus) --> verb(Numerus).
artikel(singular, m) -->  [der].
artikel(singular, w) -->  [die].
artikel(singular, n) -->  [das].
artikel(plural, _) --> [die].
subjekt(Numerus, _) --> namnam(Numerus).
namnam(singular) --> [thomas].
namnam(plural) --> [thomasense].
substantiv(singular, m) -->  [mann].
substantiv(singular, w) -->  [frau].
substantiv(singular, n) -->  [geraet].
substantiv(plural, _) --> [maenner]; [frauen].
verb(singular) --> [laeuft]; [stinkt].
verb(plural) --> [laufen].
