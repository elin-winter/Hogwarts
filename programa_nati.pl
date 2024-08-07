% Aquí va el código.
%Parte 1
%Base de conocimientos

mago(harry, mestiza, [coraje, amistad, orgullo, inteligencia]).
mago(draco, pura, [inteligencia, orgullo]).
mago(hermione, impura, [inteligencia, orgullo, responsabilidad]).

odiaCasa(harry, slytherin).
odiaCasa(draco, hufflepuff).

caracteristica(gryffindor, coraje).
caracteristica(slytherin, orgullo).
caracteristica(slytherin, inteligencia).
caracteristica(ravenclaw, inteligencia).
caracteristica(ravenclaw, responsabilidad).
caracteristica(hufflepuff, amistad).

casa(gryffindor).
casa(hufflepuff).
casa(slytherin).
casa(ravenclaw).

esMago(Persona):-
    mago(Persona, _, _).


%Punto 1

permiteEntrarMago(Casa, Mago):-
    casa(Casa),
    esMago(Mago),
    puedeEntrarCasa(Casa, Mago).

puedeEntrarCasa(Casa, _):-
    Casa \= slytherin.

puedeEntrarCasa(slytherin, Mago):-
    not(mago(Mago, impura, _)).

%Punto2

caracterApropiado(Mago, Casa):-
    casa(Casa),
    mago(Mago, _, Caracteristicas),
    forall(caracteristica(Casa, Caracteristica), member(Caracteristica, Caracteristicas)).


%Punto3

podriaQuedarSeleccionadoEn(Mago, Casa):-
    casa(Casa),
    esMago(Mago),
    caracterApropiado(Mago, Casa),
    permiteEntrarMago(Casa, Mago),
    not(odiaCasa(Mago, Casa)).

podriaQuedarSeleccionadoEn(hermione, gryffindor).

%Punto4


cadenaDeAmistades(Magos):-
    todosSonAmistosos(Magos),
    puedeEstarEnCasaSiguiente(Magos).

todosSonAmistosos(Magos):-
    forall(member(Mago, Magos), esAmistoso(Mago)).

esAmistoso(Mago):-
    mago(Mago, _, Caracteristicas),
    member(amistad, Caracteristicas).

puedeEstarEnCasaSiguiente([]).
puedeEstarEnCasaSiguiente([_]).
puedeEstarEnCasaSiguiente([Mago|Resto]):-
    head(Resto, MagoSiguiente),
    findall(Casa, podriaQuedarSeleccionadoEn(MagoSiguiente, Casa), Casas),
    forall(member(CasaSiguiente, Casas), podriaQuedarSeleccionadoEn(Mago, CasaSiguiente)),
    puedeEstarEnCasaSiguiente(Resto).

head([Cabeza|_], Cabeza).

%Parte 2

%Base de conocimientos

accion(andarFueraCamaDeNoche, -50).
accion(irTercerPiso, -75).
accion(irBosque, -50).
accion(irSeccionRestringidaBiblioteca, -10).
accion(ganarPartidaAjedrez, 50).
accion(ganarleAVoldemort, 60).
accion(usarIntelecto, 50).

accionar(harry, andarFueraCamaDeNoche).
accionar(harry, irTercerPiso).
accionar(harry, irBosque).
accionar(harry, ganarleAVoldemort).

accionar(hermione, irTercerPiso).
accionar(hermione,irSeccionRestringidaBiblioteca).
accionar(hermione, usarIntelecto).

accionar(ron, ganarPartidaAjedrez).


esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%Punto1

esBuenAlumno(Mago):-
    esMago(Mago),
    accionar(Mago, _),
    forall(accionar(Mago, Accion), buenaAccion(Accion)).

buenaAccion(Acc):-
    Puntaje > 0.

%Punto2

esRecurrente(Accion):-
    accion(Accion,_),
    accionar(Mago1, Accion),
    accionar(Mago2, Accion),
    Mago1 \= Mago2.

%Punto3

puntajeTotalCasa(Casa, PuntajeTotal):-
    casa(Casa),
    findall(Puntaje, puntajeMago(Mago, Casa, Puntaje), Puntajes),
    sum_list(Puntajes, PuntajeTotal).


puntajeMago(Mago, Casa, Puntaje):-
    esDe(Mago, Casa),
    findall(PuntajeAccion, puntosAccion)
