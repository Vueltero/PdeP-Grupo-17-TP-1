
% Punto 1

% zona(Zona).
zona(comarca).
zona(rivendel).
zona(moria).
zona(lothlorien).
zona(edoras).
zona(isengard).
zona(abismoDeHelm).
zona(minasTirith).
zona(minasMorgul).
zona(monteDelDestino).

% region(Region).
region(eriador).
region(montaniasNubladas).
region(rohan).
region(gondor).
region(mordor).

% estaEn(Zona, Region).
estaEn(comarca, eriador).
estaEn(rivendel, eriador).
estaEn(moria, montaniasNubladas).
estaEn(lothlorien, montaniasNubladas).
estaEn(edoras, rohan).
estaEn(isengard, rohan).
estaEn(abismoDeHelm, rohan).
estaEn(minasTirith, gondor).
estaEn(minasMorgul, mordor).
estaEn(monteDelDestino, mordor).

% Punto 2)
% Un camino posible podría ser:
% Comarca, Rivendel, Moria, Lothlórien, Edoras, Minas Tirith, Minas Morgul, Monte del Destino

% camino(Nombre, Lista).
camino(camino1, [comarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).
camino(camino2, [comarca, rivendel, moria, lothlorien]).
camino(camino3, [edoras, minasTirith, minasMorgul, monteDelDestino]).

% Punto 3)

sonLimitrofes(rivendel, moria).
sonLimitrofes(moria, isengard).
sonLimitrofes(lothlorien, edoras).
sonLimitrofes(edoras, minasTirith).
sonLimitrofes(minasTirith, minasMorgul).

% limitrofes(UnaZona, OtraZona).
limitrofes(UnaZona, OtraZona) :- estaEn(UnaZona, Region),
                                 estaEn(OtraZona, Region),
                                 UnaZona \= OtraZona.

limitrofes(X, Y) :- sonLimitrofes(X, Y).
limitrofes(X, Y) :- sonLimitrofes(Y, X).

% Punto 4)a)
% regionesLimitrofes(UnaRegion, OtraRegion).
regionesLimitrofes(UnaRegion, OtraRegion) :- estaEn(UnaZona, UnaRegion),
                                             estaEn(OtraZona, OtraRegion),
                                             limitrofes(UnaZona, OtraZona),
                                             UnaRegion \= OtraRegion.

% Punto 4)b)
regionesLejanas(Region1, Region2) :- region(Region1),
                                     region(Region2),
                                     Region1 \= Region2,
                                     not(regionesLimitrofes(Region1, Region2)),
                                     not( (regionesLimitrofes(Region1, Region3),
                                           regionesLimitrofes(Region2, Region3)) ).

% Punto 5)a)
puedeSeguirCon(NombreCamino, Zona) :- camino(NombreCamino, Camino),
                                last(Camino, ZonaCamino),
                                limitrofes(Zona, ZonaCamino).

% Punto 5)b)
sonConsecutivos(NombreCamino1, NombreCamino2) :- camino(NombreCamino2, Camino2),
                                                 nth1(1, Camino2, Zona2),           % Zona2 es la primer zona del camino2
                                                 puedeSeguirCon(NombreCamino1, Zona2).

% Punto 6)a)                                    






