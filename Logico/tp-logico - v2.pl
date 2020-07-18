
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

% punto 6a)
caminoLogico(Camino) :-
	camino(Camino, Zonas),
	zonasLogicas(Zonas).
	
zonasLogicas(Zonas) :-
	length(Zonas, Cantidad),
	Cantidad < 2.

zonasLogicas([Zona1, Zona2 | Zonas]) :-
	estaEn(Zona1, _),
	estaEn(Zona2, _),
	limitrofes(Zona1,Zona2),
	zonasLogicas([Zona2 | Zonas]).

%Punto 6)b)


caminoSeguro(Camino) :-
	camino(Camino, Zonas),
	zonasSeguras(Zonas).

zonasSeguras(Zonas) :-
	length(Zonas, Cantidad),
	Cantidad < 3.

zonasSeguras([Zona1, Zona2, Zona3 | Zonas]) :-
	estaEn(Zona1, Region1),
	estaEn(Zona2, Region2),
	estaEn(Zona3, Region3),
	not(
		(Region1 = Region2,
		Region2 = Region3)
		),
	zonasSeguras([Zona2, Zona3 | Zonas]).


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
    forall(camino(NombreCamino, Zonas), ultimaRegionEsMordor(Zonas)).
	
ultimaRegionEsMordor(Zonas) :-
	last(Zonas, Zona),
	estaEn(Zona, mordor).


%punto 8 a)

%viajero(Nombre, maiar(Nivel, PoderMagico)).
viajero(gandalfElGris, maiar(25, 260)).

%punto 8 b)
%viajero(Nombre, razaGuerrera(Raza, Arma, Nivel)).
viajero(legolas, razaGuerrera(elfo, arco, 29)).
viajero(legolas, razaGuerrera(elfo, espada, 20)).
viajero(gimli, razaGuerrera(enano, hacha, 26)).
viajero(aragorn, razaGuerrera(dunedain, espada, 30)).
viajero(boromir, razaGuerrera(hombre, espada, 26)).
viajero(gorbag, razaGuerrera(orco, ballesta, 24)).
viajero(ugluk, razaGuerrera(uruk-hai, espada, 26)).
viajero(ugluk, razaGuerrera(uruk-hai, arco, 22)).

%punto 8 c)
%viajero(Nombre, razaPacifica(Edad)).
viajero(frodo, razaPacifica(hobbit, 51)).
viajero(sam, razaPacifica(hobbit, 36)).
viajero(barbol, razaPacifica(ent, 5300)).


%punto 9 a)
%razaViajero/2
%razaViajero(Viajero, Raza).

razaViajero(Viajero, Raza) :-
	viajero(Viajero, Tipo),
	razaSegunTipo(Tipo, Raza).
	
razaSegunTipo(maiar(_,_), maiar).
razaSegunTipo(razaGuerrera(Raza,_,_), Raza).
razaSegunTipo(razaPacifica(Raza, _), Raza).

%punto 9 b)
%razaViajero/2
%razaViajero(Viajero, Arma).

armaViajero(Viajero, Arma) :-
	viajero(Viajero, Tipo),
	armaSegunTipo(Tipo, Arma).
	
armaSegunTipo(maiar(_,_), baston).
armaSegunTipo(razaGuerrera(_, Arma,_), Arma).
armaSegunTipo(razaPacifica(ent, _), fuerza).
armaSegunTipo(razaPacifica(hobbit, Edad), daga):-
	Edad =< 50.
armaSegunTipo(razaPacifica(hobbit, Edad), espadaCorta):-
	Edad > 50.

%punto 9 c)
%nivelViajero/2
%nivelViajero(Viajero, Nivel).

nivelViajero(Viajero, Nivel):-
	viajero(Viajero, Tipo),
	nivelSegunTipo(Viajero, Tipo, Nivel).

nivelSegunTipo(_, maiar(Nivel,_), Nivel).
nivelSegunTipo(Viajero, _, NivelDeArma) :-
	viajero(Viajero, razaGuerrera(Raza, _, NivelDeArma)),
	forall(viajero(Viajero, razaGuerrera(Raza, _, OtroNivel)), mayor(NivelDeArma, OtroNivel)).
nivelSegunTipo(_, razaPacifica(hobbit, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 3, Nivel).
nivelSegunTipo(_, razaPacifica(ent, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 100, Nivel).

mayor(UnNumero, OtroNumero) :-
	UnNumero >= OtroNumero.

calculoRazaPacifica(Edad, Divisor, Nivel) :-
	Nivel is Edad / Divisor.



% Punto 10)



% viajeros(Nombre, Lista).
viajeros(viajeros1, [gandalfElGris, legolas, gimli, aragorn, boromir, gorbag, ugluk, frodo, sam, barbol]).
viajeros(viajeros2, [gandalfElGris, legolas, gimli, aragorn, boromir]).
viajeros(viajeros3, [gorbag, ugluk, frodo, sam, barbol]).





%----------------------------------------------------------
