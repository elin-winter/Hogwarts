% ----------------- Predicados --------------------------
% ------------- Parte 1
% mago(nombre, status, caracter)
mago(harry, sangreMestiza, [corajudo, amistoso,  orgulloso, inteligente]).
mago(draco, sangrePura, [inteligente, orgulloso]).
mago(hermione, sangreImpura, [inteligente, orgullosa, responsable]).

casaOdiar(harry, slytherin).
casaOdiar(draco, hufflepuff).

caracPpal(gryffindor, [coraje]).
caracPpal(slytherin, [orgullo, inteligencia]).
caracPpal(ravenclaw, [inteligencia, responsabilidad]).
caracPpal(hufflepuff, [amistoso]).

esMago(Mago):-
    mago(Mago, _, _).

esCasa(Casa):-
    caracPpal(Casa, _). 

% Punto 1
permiteEntrar(slytherin, Mago):-
    esMago(Mago),
    not(mago(Mago, sangreImpura, _)).

permiteEntrar(Casa, Mago):-
    Casa \= slytherin,
    esCasa(Casa),
    esMago(Mago).

% Punto 2
cumpleCarac(Casa, Mago):-
    caracPpal(Casa, CaractsPpales),
    mago(Mago, _, Caracter),
    forall(member(Carac, CaractsPpales), member(Carac, Caracter)).

% Punto 3
casasPosible(Mago, Casas) :-
    esMago(Mago),
    findall(Casa, casaPosible(Mago, Casa), CasasPosibles),
    incluirCasaEspecial(Mago, CasasPosibles, Casas).

casaPosible(Mago, Casa) :-
    permiteEntrar(Casa, Mago),
    cumpleCarac(Casa, Mago),
    not(casaOdiar(Mago, Casa)).

incluirCasaEspecial(hermione, CasasPosibles, [gryffindor | CasasPosibles]).
incluirCasaEspecial(_, CasasPosibles, CasasPosibles).

% Punto 4
cadenaDeAmistades([]).
cadenaDeAmistades([_]).
cadenaDeAmistades([Mago1, Mago2 | Magos]) :-
    amistoso(Mago1),
    amistoso(Mago2),
    casasPosible(Mago1, Casas1),
    casasPosible(Mago2, Casas2),
    intersection(Casas1, Casas2, CasasComun),
    CasasComun \= [],
    cadenaDeAmistades([Mago2 | Magos]).

amistoso(Mago) :-
    mago(Mago, _, Caracteristicas),
    member(amistoso, Caracteristicas).
    
% ------------- Parte 2
acciones(andarDeNoche, -50).
acciones(irALugar(Lugar), Puntaje):-
    prohibido(Lugar, Puntaje).

prohibido(bosque, -50).
prohibido(seccionRestringidaBiblioteca, -10).
prohibido(tercerPiso, -75).

acciones(ganarPartidaAjedrez, 50).
acciones(salvarAmigos,50).
acciones(vencerVoldemort, 60).

accion(harry, andarDeNoche).
accion(harry, irALugar(bosque)).
accion(harry,irALugar(tercerPiso)).
accion(hermione, irALugar(tercerPiso)).
accion(hermionse, irALugar(seccionRestringidaBiblioteca)).
accion(draco, irALugar(mazmorras)).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% Punto 1A
buenAlumno(Mago):-
    esMago(Mago),
    accion(Mago, _),
    forall(accion(Mago, Accion), not((acciones(Accion, Valor), Valor < 0))).

% Punto 1B
accionRecurrente(Accion):-
    accion(Mago1, Accion),
    accion(Mago2, Accion),
    Mago1 \= Mago2.

% Punto 2
puntajeTotalCasa(Casa, Puntaje):-
    esCasa(Casa),
    findall(PuntajeAlumno, 
        (esDe(Mago, Casa), puntajeAlumno(Mago, PuntajeAlumno)), 
        PuntajesAlumnos),
    sumlist(PuntajesAlumnos, Puntaje).

puntajeAlumno(Mago, PuntajeMago):-
    esMago(Mago),
    findall(Puntaje, 
        (accion(Mago, Accion), acciones(Accion, Puntaje)),
        Puntajes),
    sumlist(Puntajes, PuntajeMago).

% Punto 3
casaGanadora(CasaGanadora):-
    findall(Puntaje, puntajeTotalCasa(Casa, Puntaje), PuntajesCasas),
    max_member(MaxPuntaje, PuntajesCasas),
    puntajeTotalCasa(CasaGanadora, MaxPuntaje).

% Punto 4

acciones(pregunta(_, Dificultad, _), Dificultad).
accion(hermione, pregunta("¿Dónde se encuentra un Bezoar?", 20, snape)).
accion(hermione, pregunta("¿Cómo levitar una pluma?", 25, flitwick)).

    








