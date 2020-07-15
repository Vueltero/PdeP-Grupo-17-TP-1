%pertenece(ZonaPerteneciente,RegionALaQuePertenece)
pertenece(comarca,eriador).
pertenece(rivendel,eriador).
pertenece(moria,montaniasNubladas).
pertenece(lothlorien,montaniasNubladas).
pertenece(edoras,rohan).
pertenece(isengard,rohan).
pertenece(abismoDeHelm,rohan).
pertenece(minasTirith,gondor).
pertenece(minasMorgul,mordor).
pertenece(monteDelDestino,mordor).
not(pertenece(fangorn,gondor)).

%camino([Zonas])
camino([comarca,rivendel,moria,lothlorien,edoras,minasTirith,minasMorgul,monteDelDestino]).

%zonaslimitrofes(Zona1,Zona2)
zonasLimitrofes(rivendel,moria).
zonasLimitrofes(moria,rivendel).
zonasLimitrofes(moria,isengard).
zonasLimitrofes(isengard,moria).
zonasLimitrofes(lothlorien,edoras).
zonasLimitrofes(edoras,lothlorien).
zonasLimitrofes(edoras,minasTirith).
zonasLimitrofes(minasTirith,edoras).
zonasLimitrofes(minasTirith,minasMorgul).
zonasLimitrofes(minasMorgul,minasTirith).
zonasLimitrofes(Zona1,Zona2):-
    pertenece(Zona1,Region),
    pertenece(Zona2,Region),
    Zona1\=Zona2.

regionesLimitrofes(Region1,Region2):-   
    pertenece(Zona1,Region1),
    pertenece(Zona2,Region2),
    Region1\=Region2,
    zonasLimitrofes(Zona1,Zona2).

%regionesLejanas(Region1,Region2) Tira false por default %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
regionesLejanas(Region1,Region2):-
    pertenece(_,Region1),
    pertenece(_,Region2),
    not(regionesLimitrofes(Region1,Region2)),
    not(tienenRegionPuente(Region1,Region2)).

%tienenRegionPuente(Region1,Region2)
tienenRegionPuente(Region1,Region2):-
    regionesLimitrofes(Region1,Region3),
    regionesLimitrofes(Region2,Region3),
    Region1\=Region3,
    Region2\=Region3,
    Region1\=Region2.

%puedeSeguirCon(Camino,Zona)
puedeSeguirCon(Camino,Zona):-
    last(Camino,Last),
    zonasLimitrofes(Last,Zona).

%sonConsecutivos(Camino1,Camino2)
sonConsecutivos(Camino1,Camino2):-   
    nth1(1,Camino2,Zona2),
    puedeSeguirCon(Camino1,Zona2).

%tieneSentido(Camino) Solo sirve si no se repiten zonas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tieneSentido(Camino):-
    nth1(1,Camino,X),
    nth1(2,Camino,Y),
    puedeSeguirCon([X],Y),
    delete(Camino,X,CaminoMenos1),
    tieneSentido(CaminoMenos1).
tieneSentido([Zona]):-
    pertenece(Zona,_).

caminoSeguro(Camino):-  
    length(Camino,Longitud),
    Longitud=<2.
caminoSeguro(Camino):-%Solo sirve para caminos de hasta 4 de longitud %%%%%%%%%%%%%%%%%%%%%%%
    nth1(2,Camino,Zona2),
    nth1(3,Camino,Zona3),
    pertenece(Zona2,Region2),
    pertenece(Zona3,Region3),
    Region2\=Region3.