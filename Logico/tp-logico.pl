
% Punto 1)

% zona(Zona).
% Quizas este predicado zona/1 no es util. Se podria sacar
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
tieneLogica(NombreCamino):-
    longitudCamino(NombreCamino,Longitud),
    Longitud>=2,
    camino(NombreCamino,ListaCaminos),
    forall((member(Zona1,ListaCaminos),nextto(Zona1,Zona2,ListaCaminos)),limitrofes(Zona1,Zona2)).
tieneLogica(NombreCamino):-
    longitudCamino(NombreCamino,Longitud),
    Longitud<2.

longitudCamino(NombreCamino,Longitud):- 
    camino(NombreCamino,ListaCaminos),
    length(ListaCaminos,Longitud).

%Punto 6)b)





% Punto 7)a)
regionesDelCamino(NombreCamino, Conjunto) :- 
    camino(NombreCamino, Camino),
    findall(Region, (estaEn(Zona, Region), member(Zona,Camino)), Conjunto).

cantidadDeRegiones(NombreCamino, CantidadRegiones) :-
    regionesDelCamino(NombreCamino, ConjuntoDeRegiones),
    list_to_set(ConjuntoDeRegiones, Conjunto),
    length(Conjunto, CantidadRegiones).

% Punto 7)b)
todosLosCaminosConducenAMordor(NombreCamino) :-     % Esta mal hecho. Es inversible.
    camino(NombreCamino, Camino),
    last(Camino, Zona),
    estaEn(Zona, mordor).



% Punto 8)a)

% viajero(raza(maiar), nombre(Nombre), nivel(Nivel), poderMagico(NivelPoderMagico)).
viajero(raza(maiar), nombre(gandalfElGris), nivel(25), poderMagico(260)).

% Punto 8)b)
% i)
% viajero(raza(elfo), nombre(Nombre), arco(NivelArco), espada(NivelEspada)).
viajero(raza(elfo), nombre(legolas), arco(29), espada(20)).

% ii)
% viajero(raza(enano), nombre(Nombre), hacha(NivelHacha)).
viajero(raza(enano), nombre(gimli), hacha(26)).

% iii)
% viajero(raza(dunedain), nombre(Nombre), espada(NivelEspada)).
viajero(raza(dunedain), nombre(aragorn), espada(30)).

% iv)
% (raza(hombre), nombre(Nombre), espada(NivelEspada)).
viajero(raza(hombre), nombre(boromir), espada(26)).

% v)
% viajero(raza(orco), nombre(Nombre), ballesta(NivelBallesta)).
viajero(raza(orco), nombre(gorbag), ballesta(24)).

% vi)
% viajero(raza(uruk_hai), nombre(Nombre), espada(NivelEspada), arco(NivelArco)).
viajero(raza(uruk_hai), nombre(ugluk), espada(26), arco(22)).

% Punto 8)c)
% viajero(raza(hobbit), nombre(Nombre), edad(Edad)).

% i)
viajero(raza(hobbit), nombre(frodo), edad(51)).

% ii)
viajero(raza(hobbit), nombre(sam), edad(36)).

% iii)
viajero(raza(ent), nombre(barbol), edad(5300)).


% Punto 9)a)
esDeRaza(viajero(raza(Raza), _, _), Raza).              % para viajero/3
esDeRaza(viajero(raza(Raza), _, _, _), Raza).           % para viajero/4

% Punto 9)b)
%                    viajero          arma
armasQueManeja(viajero(raza(maiar)), baston).
armasQueManeja(viajero(raza(hobbit), Nombre, edad(Edad)), daga) :-
    viajero(raza(hobbit), Nombre, edad(Edad)),
    forall(viajero(raza(hobbit), Nombre, edad(Edad)), Edad =< 50).
armasQueManeja(viajero(raza(hobbit), Nombre, edad(Edad)), espadaCorta) :-
    viajero(raza(hobbit), Nombre, edad(Edad)),
    forall(viajero(raza(hobbit), Nombre, edad(Edad)), Edad > 50).
armasQueManeja(viajero(raza(ent)), fuerza).


% Punto 9)c)
% i)
suNivel(viajero(raza(maiar), nombre(gandalfElGris)), 25).

% ii)




% iii)
suNivel(viajero(raza(hobbit), nombre(Nombre)), Nivel) :-
    viajero(raza(hobbit), nombre(Nombre), edad(Edad)),
    Nivel is Edad / 3.

suNivel(viajero(raza(ent), nombre(Nombre)), Nivel) :- 
    viajero(raza(ent), nombre(Nombre), edad(Edad)),
    Nivel is Edad / 100.


/*
raza guerrera (elfo, enano, dúnedain, hombre, orco o uruk-hai)
*/




% Punto 10)



% viajeros(Nombre, Lista).
viajeros(viajeros1, [gandalfElGris, legolas, gimli, aragorn, boromir, gorbag, ugluk, frodo, sam, barbol]).
viajeros(viajeros2, [gandalfElGris, legolas, gimli, aragorn, boromir]).
viajeros(viajeros3, [gorbag, ugluk, frodo, sam, barbol]).





%----------------------------------------------------------
% Faltan punto 6)b) 7)b)
% El punto 7)b) esta mal hecho