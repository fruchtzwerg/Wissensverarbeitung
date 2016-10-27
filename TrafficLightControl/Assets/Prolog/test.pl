gewicht(d,1). 
gewicht(a,10). 
gewicht(b,2). 
gewicht(c,15).
 
gewichtsumme([], 0). 
gewichtsumme([B|R], G):-    
            gewichtsumme(R, ZG),    
            gewicht(B,Y),    
            G is ZG + Y.
 
gewicht_auf(Buch, Gewicht):-    
            listAbove(Buch, L),    
            gewichtsumme(L, Gewicht).
