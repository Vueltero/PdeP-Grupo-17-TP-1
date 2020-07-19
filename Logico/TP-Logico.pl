
%%%%%%%%%%%%%%%%%%%% PUNTO 1 %%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%% PUNTO 2 %%%%%%%%%%%%%%%%%%%%

% Un camino posible podría ser:
% Comarca, Rivendel, Moria, Lothlórien, Edoras, Minas Tirith, Minas Morgul, Monte del Destino

% camino(Nombre, Lista).
camino(camino1, [comarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).
camino(camino2, [comarca, rivendel, moria, lothlorien]).
camino(camino3, [edoras, minasTirith, minasMorgul, monteDelDestino]).


%%%%%%%%%%%%%%%%%%%% PUNTO 3 %%%%%%%%%%%%%%%%%%%%

sonLimitrofes(rivendel, moria).
sonLimitrofes(moria, isengard).
sonLimitrofes(lothlorien, edoras).
sonLimitrofes(edoras, minasTirith).
sonLimitrofes(minasTirith, minasMorgul).

% limitrofes(UnaZona, OtraZona).
limitrofes(UnaZona, OtraZona) :- 
	estaEn(UnaZona, Region),
    estaEn(OtraZona, Region),
	UnaZona \= OtraZona.

limitrofes(X, Y) :- sonLimitrofes(X, Y).
limitrofes(X, Y) :- sonLimitrofes(Y, X).


%%%%%%%%%%%%%%%%%%%% PUNTO 4 %%%%%%%%%%%%%%%%%%%%

%% a) regionesLimitrofes(UnaRegion, OtraRegion).
regionesLimitrofes(UnaRegion, OtraRegion) :- 
	estaEn(UnaZona, UnaRegion),
    estaEn(OtraZona, OtraRegion),
	limitrofes(UnaZona, OtraZona),
    UnaRegion \= OtraRegion.

%% b)
regionesLejanas(Region1, Region2) :- 
	region(Region1),
    region(Region2),
    Region1 \= Region2,
    not(regionesLimitrofes(Region1, Region2)),
    not((regionesLimitrofes(Region1, Region3),
         regionesLimitrofes(Region2, Region3))).


%%%%%%%%%%%%%%%%%%%% PUNTO 5 %%%%%%%%%%%%%%%%%%%%

%% a)
puedeSeguirCon(NombreCamino, Zona) :- 
	camino(NombreCamino, Camino),
    last(Camino, ZonaCamino),
    limitrofes(Zona, ZonaCamino).

%% b)
sonConsecutivos(NombreCamino1, NombreCamino2) :- 
	camino(NombreCamino2, Camino2),
    nth1(1, Camino2, Zona2),           % Zona2 es la primer zona del camino2
    puedeSeguirCon(NombreCamino1, Zona2).


%%%%%%%%%%%%%%%%%%%% PUNTO 6 %%%%%%%%%%%%%%%%%%%%

%% a)
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

%% b)
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
	not((Region1 = Region2, Region2 = Region3)),
	zonasSeguras([Zona2, Zona3 | Zonas]).


%%%%%%%%%%%%%%%%%%%% PUNTO 7 %%%%%%%%%%%%%%%%%%%%

%% a)

regionesDelCamino(NombreCamino, Conjunto) :- 
    camino(NombreCamino, Camino),
    findall(Region, (estaEn(Zona, Region), member(Zona,Camino)), Conjunto).

cantidadDeRegiones(NombreCamino, CantidadRegiones) :-
    regionesDelCamino(NombreCamino, ConjuntoDeRegiones),
    list_to_set(ConjuntoDeRegiones, Conjunto),
    length(Conjunto, CantidadRegiones).

%% b)
todosLosCaminosConducenAMordor(NombreCamino) :- 
    forall(camino(NombreCamino, Zonas), ultimaRegionEsMordor(Zonas)).
	
ultimaRegionEsMordor(Zonas) :-
	last(Zonas, Zona),
	estaEn(Zona, mordor).



%Modelar a los viajeros, sabiendo que cada viajero tiene un nombre y:
%De un maiar, se conoce su nivel y su poder mágico.
%Modelar a Gandalf (el gris), de nivel 25 y poder mágico 260.


%De un viajero de una raza guerrera (elfo, enano, dúnedain, hombre, orco o uruk-hai), se conoce la raza en sí y las armas que tiene, junto con el nivel de manejo que tiene con cada una.
%Modelar a:
%Légolas, que es un elfo y maneja un arco a nivel 29 y una espada a nivel 20.
%Gimli, que es un enano que maneja un hacha a nivel 26.
%Aragorn, que es un dúnedain que maneja una espada a nivel 30.
%Boromir, que es un hombre que maneja una espada a nivel 26.
%Gorbag, que es un orco que maneja una ballesta a nivel 24.
%Úgluk, que es un uruk-hai que maneja una espada a nivel 26 y un arco a nivel 22.


%De un viajero de raza pacifista (hobbit o ent), se conoce la edad.
%Modelar a:
%Frodo, que es un hobbit de 51 años.
%Sam, que es un hobbit de 36 años.
%Bárbol, que es un ent de 5300 años.


%%%%%%%%%%%%%%%%%%%% PUNTO 8 %%%%%%%%%%%%%%%%%%%%

%% a)

%viajero(Nombre, maiar(Nivel, PoderMagico)).
%viajero(Nombre, razaGuerrera(Raza, Arma, Nivel)).
%viajero(Nombre, razaPacifica(Raza, Edad)).

viajero(gandalfElGris, maiar(25, 260)).

%% b)

viajero(legolas, razaGuerrera(elfo, [arma(arco, 29), arma(espada, 20)])).
viajero(gimli, razaGuerrera(enano, [arma(hacha, 26)])).
viajero(aragorn, razaGuerrera(dunedain, [arma(espada, 30)])).
viajero(boromir, razaGuerrera(hombre, [arma(espada, 26)])).
viajero(gorbag, razaGuerrera(orco, [arma(ballesta, 24)])).
viajero(ugluk, razaGuerrera(uruk-hai, [arma(espada, 26), arma(arco, 22)])).

%% c)

viajero(frodo, razaPacifica(hobbit, 51)).
viajero(sam, razaPacifica(hobbit, 36)).
viajero(barbol, razaPacifica(ent, 5300)).

%tieneElemento(NombrePersona, Elemento).
tieneElemento(legolas, [panDeLembasDelorien, panDeLembasDelorien]).


%%%%%%%%%%%%%%%%%%%% PUNTO 9 %%%%%%%%%%%%%%%%%%%%

%Relacionar a los viajeros con distintas características que le son propias:
%A un viajero con su raza.

%A un viajero con un arma que maneja. Un maiar usa un bastón. Los hobbits manejan una daga hasta los 50 años, y luego una espada corta.
%Para los ents, en cambio, su arma es su fuerza... ¡créannos que tiene que considerarse un arma!


%A un viajero con su nivel.
%Del maiar, se conoce explícitamente, como ya dijimos.
%De las razas guerreras (cualquiera sea), es el nivel máximo de manejo de un arma que tenga.
%De las razas pacifistas, es directamente proporcional a la edad:
%En el caso de los hobbits, la tercera parte de su edad.
%En cuanto a los ents, es la centésima parte de su edad.

%% a)

%razaViajero/2
razaViajero(Viajero, Raza) :-
	viajero(Viajero, Tipo),
	razaSegunTipo(Tipo, Raza).
	
razaSegunTipo(maiar(_,_), maiar).
razaSegunTipo(razaGuerrera(Raza,_), Raza).
razaSegunTipo(razaPacifica(Raza, _), Raza).

%% b)

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

nivelViajero(Viajero, Nivel):-
	viajero(Viajero, Tipo),
	nivelSegunTipo(Viajero, Tipo, Nivel).

nivelSegunTipo(_, maiar(Nivel,_), Nivel).
nivelSegunTipo(Viajero, _, NivelDeArma) :-
	viajero(Viajero, razaGuerrera(_, Armas)),
	nivelDeArmas(Armas, NivelDeArma),
	forall(nivelDeArmas(Armas, OtroNivel), (NivelDeArma >= OtroNivel)).
nivelSegunTipo(_, razaPacifica(hobbit, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 3, Nivel).
nivelSegunTipo(_, razaPacifica(ent, Edad), Nivel) :-
	calculoRazaPacifica(Edad, 100, Nivel).

nivelDeArmas(Armas, Nivel) :-
	member(arma(_,Nivel), Armas).


calculoRazaPacifica(Edad, Divisor, Nivel) :-
	Nivel is Edad / Divisor.
	
	
%grupo(grupo1, [legolas, gandalfElGris]).

grupo(grupo1, legolas).
grupo(grupo1, gandalfElGris).


%%%%%%%%%%%%%%%%%%%% PUNTO 10 %%%%%%%%%%%%%%%%%%%%

%Hacer puedeAtravesar/2 que relaciona una zona y cualquier lista de viajeros (o grupo) posible en la cual dichos viajeros pueden atravesar la zona. 
%Para poder atravesarla, un grupo debe cumplir con todos los requerimientos de la zona.

grupoCumpleConPersonaje(Grupo, RazaRequerida, NivelRequerido) :-
	grupo(Grupo, Integrante),
	razaViajero(Integrante, RazaRequerida),
	nivelViajero(Integrante, Nivel),
	Nivel >= NivelRequerido.

grupoCumpleConElemento(Grupo, Item, CantidadRequerida) :-
	grupo(Grupo, _),
	cantidadDeUnElemento(Item, Grupo, Cantidad),
	Cantidad >= CantidadRequerida.

cantidadDeUnElemento(Item, Grupo, Cantidad) :-
	grupo(Grupo, _),
	findall(Item, integranteTieneItem(Grupo, Item), CantidadDeItems),
	length(CantidadDeItems, Cantidad).

integranteTieneItem(Grupo, Item) :-
	grupo(Grupo, Integrante),
	tieneElemento(Integrante, Elementos),
	member(Item, Elementos).

grupoCumpleConMagia(Grupo, CantMagiaRequerida) :-
	grupo(Grupo, _),
	findall(PoderMagico, poderMagicoDelIntegrante(Grupo, PoderMagico), PoderesMagicos),
	sum_list(PoderesMagicos, Cantidad),
	Cantidad >= CantMagiaRequerida.

poderMagicoDelIntegrante(Grupo, PoderMagico) :-
	grupo(Grupo, Integrante),
	poderMagico(Integrante, PoderMagico).

poderMagico(Viajero, Cantidad) :-
	viajero(Viajero, _),
	razaViajero(Viajero, Raza),
	poderMagicoSegunRaza(Viajero, Raza, Cantidad).


%De los requerimientos de magia, también para un grupo, se conoce el poder mágico total mínimo necesario. 
%Los elfos tienen un poder mágico igual al doble de su nivel, y los dunedain y enanos a su vez tienen igual poder que su nivel.


poderMagicoSegunRaza(Viajero, maiar, Cantidad) :-
	viajero(Viajero, maiar(_, Cantidad)).
	
poderMagicoSegunRaza(Viajero, elfo, Cantidad) :-
	nivelViajero(Viajero, Nivel),
	Cantidad is Nivel * 2.
	
poderMagicoSegunRaza(Viajero, enano, Cantidad) :-
	nivelViajero(Viajero, Cantidad).
	
poderMagicoSegunRaza(Viajero, dunedain, Cantidad) :-
	nivelViajero(Viajero, Cantidad).


requerimientoDePersonaje(moria, maiar, 24).
requerimientoDePersonaje(isengard, maiar, 27).
requerimientoDePersonaje(isengard, elfo, 30).

requerimientoDeElementos(moria, cotaDeMallaMithril, 1).
requerimientoDeElementos(moria, panDeLembasDelorien, 2).

requerimientoDeMagia(moria, 230).

puedeAtravesar(Grupo, Zona) :-
	grupo(Grupo, _),
	requerimientosDeZona(Zona, Grupo).


requerimientosDeZona(Lugar, Grupo) :-
	grupo(Grupo, _),
	requerimientoDePersonaje(Lugar, Raza, Nivel),
	requerimientoDeElementos(Lugar, Item, Cantidad),
	requerimientoDeMagia(Lugar, CantidadDeMagia),
	grupoCumpleConPersonaje(Grupo, Raza, Nivel),
	grupoCumpleConElemento(Grupo, Item, Cantidad),
	grupoCumpleConMagia(Grupo, CantidadDeMagia).
	

%%%%%%%%%%%%%%%%%%%% PUNTO 11 %%%%%%%%%%%%%%%%%%%%

%Hacer seSientenComoEnCasa/2 que relaciona a un grupo y a una región, si el grupo puede atravesar cualquier zona de dicha región.

seSientenComoEnCasa(Grupo, Region) :-
	grupo(Grupo, _),
	estaEn(_, Region),
	forall(estaEn(Zona, Region), puedeAtravesar(Grupo, Zona)).
