
%%%%%%%%%%%%%%%%%%%% PUNTO 1 %%%%%%%%%%%%%%%%%%%%

%estaEn/2
%estaEn(Zona, Region).

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


%%%%%%%%%%%%%%%%%%%% PUNTO 2 %%%%%%%%%%%%%%%%%%%%

% Un camino posible podría ser:
% Comarca, Rivendel, Moria, Lothlórien, Edoras, Minas Tirith, Minas Morgul, Monte del Destino

%camino/2
% camino(Nombre, Lista).

camino(camino1, [comarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).
camino(camino2, [comarca, rivendel, moria, lothlorien]).
camino(camino3, [edoras, minasTirith, minasMorgul, monteDelDestino]).


%%%%%%%%%%%%%%%%%%%% PUNTO 3 %%%%%%%%%%%%%%%%%%%%

%sonLimitrofes/2
%sonLimitrofes(Zona, Region).

sonLimitrofes(rivendel, moria).
sonLimitrofes(moria, isengard).
sonLimitrofes(lothlorien, edoras).
sonLimitrofes(edoras, minasTirith).
sonLimitrofes(minasTirith, minasMorgul).

%limitrofes/2
%limitrofes(UnaZona, OtraZona).
limitrofes(UnaZona, OtraZona) :- 
	estaEn(UnaZona, Region),
    estaEn(OtraZona, Region),
	UnaZona \= OtraZona.

limitrofes(X, Y) :- sonLimitrofes(X, Y).
limitrofes(X, Y) :- sonLimitrofes(Y, X).


%%%%%%%%%%%%%%%%%%%% PUNTO 4 %%%%%%%%%%%%%%%%%%%%

%% a)

%regionesLimitrofes/2
%regionesLimitrofes(UnaRegion, OtraRegion).

regionesLimitrofes(UnaRegion, OtraRegion) :- 
	estaEn(UnaZona, UnaRegion),
	limitrofes(UnaZona, OtraZona),
   	estaEn(OtraZona, OtraRegion).

%% b)

%regionesLejanas/2
%regionesLejanas(Region1, Region2).

regionesLejanas(Region1, Region2) :- 
	estaEn(_, Region1),
	estaEn(_, Region2),
	Region1 \= Region2,
	not(regionesLimitrofes(Region1, Region2)),
	not(regionEnComun(Region1, Region2)).
	
regionEnComun(Region1, Region2) :-
	regionesLimitrofes(Region1, Region3),
    regionesLimitrofes(Region2, Region3).


%%%%%%%%%%%%%%%%%%%% PUNTO 5 %%%%%%%%%%%%%%%%%%%%

%% a)

%puedeSeguirCon/2
%puedeSeguirCon(NombreCamino, Zona).

puedeSeguirCon(NombreCamino, Zona) :- 
	camino(NombreCamino, Camino),
    last(Camino, ZonaCamino),
    limitrofes(Zona, ZonaCamino).

%% b)

%sonConsecutivos/2
%sonConsecutivos(NombreCamino1, NombreCamino2).

sonConsecutivos(NombreCamino1, NombreCamino2) :- 
	camino(NombreCamino2, Camino2),
	nth1(1, Camino2, PrimerZonaCamino2),
	puedeSeguirCon(NombreCamino1, PrimerZonaCamino2).


%%%%%%%%%%%%%%%%%%%% PUNTO 6 %%%%%%%%%%%%%%%%%%%%

%% a)

%caminoLogico/1
%caminoLogico(Camino)

caminoLogico(Camino) :-
	camino(Camino, Zonas),
	zonasLogicas(Zonas).
	
zonasLogicas(Zonas) :-
	length(Zonas, Cantidad),
	Cantidad < 2.

zonasLogicas([Zona1, Zona2 | Zonas]) :-
	limitrofes(Zona1,Zona2),
	zonasLogicas([Zona2 | Zonas]).

%% b)

%caminoSeguro/1
%caminoSeguro(Camino).

caminoSeguro(Camino) :-
	camino(Camino, Zonas),
	zonasSeguras(Zonas).

zonasSeguras(Zonas) :-
	length(Zonas, Cantidad),
	Cantidad < 3.

zonasSeguras([Zona1, Zona2, Zona3 | Zonas]) :-
	estaEn(Zona1, Region),
	estaEn(Zona2, Region),
	estaEn(Zona3, Region),
	zonasSeguras([Zona2, Zona3 | Zonas]).


%%%%%%%%%%%%%%%%%%%% PUNTO 7 %%%%%%%%%%%%%%%%%%%%

%% a)

%cantidadDeRegiones/2
%cantidadDeRegiones(NombreCamino, CantidadRegiones).

cantidadDeRegiones(NombreCamino, CantidadRegiones) :-
    regionesDelCamino(NombreCamino, ConjuntoDeRegiones),
    list_to_set(ConjuntoDeRegiones, Conjunto),
    length(Conjunto, CantidadRegiones).

regionesDelCamino(NombreCamino, Conjunto) :- 
    camino(NombreCamino, Camino),
    findall(Region, (estaEn(Zona, Region), member(Zona,Camino)), Conjunto).


%% b)
todosLosCaminosConducenAMordor(NombreCamino) :- 
    forall(camino(NombreCamino, Zonas), ultimaRegionEsMordor(Zonas)).
	
ultimaRegionEsMordor(Zonas) :-
	last(Zonas, Zona),
	estaEn(Zona, mordor).


%%%%%%%%%%%%%%%%%%%% PUNTO 8 %%%%%%%%%%%%%%%%%%%%

%% a)

%viajero(Nombre, maiar(Nivel, PoderMagico)).

viajero(gandalfElGris, maiar(25, 260)).

%% b)

%viajero(Nombre, razaGuerrera(Raza, Arma, Nivel)).

viajero(legolas, razaGuerrera(elfo, [arma(arco, 29), arma(espada, 20)])).
viajero(gimli, razaGuerrera(enano, [arma(hacha, 26)])).
viajero(aragorn, razaGuerrera(dunedain, [arma(espada, 30)])).
viajero(boromir, razaGuerrera(hombre, [arma(espada, 26)])).
viajero(gorbag, razaGuerrera(orco, [arma(ballesta, 24)])).
viajero(ugluk, razaGuerrera(uruk-hai, [arma(espada, 26), arma(arco, 22)])).

%% c)

%viajero(Nombre, razaPacifica(Raza, Edad)).

viajero(frodo, razaPacifica(hobbit, 51)).
viajero(sam, razaPacifica(hobbit, 36)).
viajero(barbol, razaPacifica(ent, 5300)).

%tieneElemento/2
%tieneElemento(NombrePersona, Elemento).
tieneElemento(legolas, [panDeLembasDelorien, panDeLembasDelorien]).


%%%%%%%%%%%%%%%%%%%% PUNTO 9 %%%%%%%%%%%%%%%%%%%%


%% a)

%razaViajero/2
%razaViajero(Viajero, Raza).

razaViajero(Viajero, Raza) :-
	viajero(Viajero, Tipo),
	razaSegunTipo(Tipo, Raza).
	
razaSegunTipo(maiar(_,_), maiar).
razaSegunTipo(razaGuerrera(Raza,_), Raza).
razaSegunTipo(razaPacifica(Raza, _), Raza).

%% b)

%razaViajero/2
%razaViajero(Viajero,Arma).

armaViajero(Viajero, Arma) :-
	viajero(Viajero, Tipo),
	armaSegunTipo(Tipo, Arma).
	
armaSegunTipo(maiar(_,_), baston).
armaSegunTipo(razaGuerrera(_, Armas), Arma) :-
	member(arma(Arma,_), Armas).
armaSegunTipo(razaPacifica(ent, _), fuerza).
armaSegunTipo(razaPacifica(hobbit, Edad), daga):-
	Edad =< 50.
armaSegunTipo(razaPacifica(hobbit, Edad), espadaCorta):-
	Edad > 50.
	
%% c)

%nivelViajero/2
%nivelViajero(Viajero, Nivel).

nivelViajero(Viajero, Nivel):-
	viajero(Viajero, Tipo),
	nivelSegunTipo(Tipo, Nivel).

nivelSegunTipo(maiar(Nivel,_), Nivel).
nivelSegunTipo(razaGuerrera(_, Armas), NivelDeArma) :-
	nivelDeArmas(Armas, NivelDeArma),
	forall(nivelDeArmas(Armas, OtroNivel), (NivelDeArma >= OtroNivel)).
nivelSegunTipo(razaPacifica(hobbit, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 3, Nivel).
nivelSegunTipo(razaPacifica(ent, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 100, Nivel).

nivelDeArmas(Armas, Nivel) :-
	member(arma(_,Nivel), Armas).


calculoRazaPacifica(Edad, Divisor, Nivel) :-
	Nivel is Edad / Divisor.
	
	
%grupo(grupo1, [legolas, gandalfElGris]).

grupo(grupo1, legolas).
grupo(grupo1, gandalfElGris).


%%%%%%%%%%%%%%%%%%%% PUNTO 10 %%%%%%%%%%%%%%%%%%%%

requerimiento(moria, personaje(maiar, 24)).
requerimiento(moria, elemento(cotaDeMallaMithril, 1)).
requerimiento(moria, poder(200)).


%puedeAtravesar/2
%puedeAtravesar(Grupo, Zona).

puedeAtravesar(Grupo, Zona) :-
	grupo(Grupo, _),
	estaEn(Zona, _),
	forall(requerimiento(Zona, Requerimiento), cumpleCon(Grupo, Requerimiento)).

cumpleCon(Grupo, personaje(RazaRequerida, NivelRequerido)) :-
	grupo(Grupo, Miembro),
	razaViajero(Miembro, RazaRequerida),
	nivelViajero(Miembro, Nivel),
	Nivel >= NivelRequerido.

cumpleCon(Grupo, elemento(Elemento, CantidadRequerida)) :-
	findall(Elemento, grupoTieneItem(Grupo, Elemento), Elementos),
	length(Elementos, Cantidad),
	Cantidad >= CantidadRequerida.

grupoTieneItem(Grupo, Item) :-
	grupo(Grupo, Integrante),
	tieneElemento(Integrante, Elementos),
	member(Item, Elementos).

cumpleCon(Grupo, poder(CantidadRequerida)) :-
	findall(PoderMagico, poderMagicoDeGrupo(Grupo, PoderMagico), PoderesMagicos),
	sum_list(PoderesMagicos, CantidadTotalDeMagia),
	CantidadTotalDeMagia >= CantidadRequerida.

poderMagicoDeGrupo(Grupo, PoderMagico) :-
	grupo(Grupo, Integrante),
	poderMagico(Integrante, PoderMagico).

poderMagico(Viajero, Cantidad) :-
	viajero(Viajero, _),
	razaViajero(Viajero, Raza),
	poderMagicoSegunRaza(Viajero, Raza, Cantidad).

poderMagicoSegunRaza(Viajero, maiar, Cantidad) :-
	viajero(Viajero, maiar(_, Cantidad)).
	
poderMagicoSegunRaza(Viajero, elfo, Cantidad) :-
	nivelViajero(Viajero, Nivel),
	Cantidad is Nivel * 2.
	
poderMagicoSegunRaza(Viajero, enano, Cantidad) :-
	nivelViajero(Viajero, Cantidad).
	
poderMagicoSegunRaza(Viajero, dunedain, Cantidad) :-
	nivelViajero(Viajero, Cantidad).


%%%%%%%%%%%%%%%%%%%% PUNTO 11 %%%%%%%%%%%%%%%%%%%%

%seSientenComoEnCasa/2
%seSienteComoEnCasa(Grupo, Region).

seSientenComoEnCasa(Grupo, Region) :-
	grupo(Grupo, _),
	estaEn(_, Region),
	forall(estaEn(Zona, Region), puedeAtravesar(Grupo, Zona)).
