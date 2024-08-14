% ----------------- Predicados --------------------------
% ------------- Parte 1

statusSangre(harry, mestiza).
statusSangre(draco, pura).
statusSangre(hermione, impura).

magoEs(harry, coraje).
magoEs(harry, amistad).
magoEs(harry, orgullo).
magoEs(harry, inteligencia).
magoEs(draco, inteligencia).
magoEs(draco, orgullo).
magoEs(hermione, inteligencia).
magoEs(hermione, orgullo).
magoEs(hermione, responsabilidad).

casaOdiar(harry, slytherin).
casaOdiar(draco, hufflepuff).

caracterCasa(gryffindor, coraje).
caracterCasa(slytherin, orgullo).
caracterCasa(slytherin, inteligencia).
caracterCasa(ravenclaw, inteligencia).
caracterCasa(ravenclaw, responsabilidad).
caracterCasa(hufflepuff, amistad).

mago(Mago):-
    magoEs(Mago, _).

mago(Mago):-
    esDe(Mago, _).

casa(Casa):-
    caracterCasa(Casa, _). 

% Punto 1
permiteEntrar(Casa, Mago):-
    casa(Casa),
    mago(Mago),
    condicionSly(Casa, Mago).

condicionSly(slytherin, Mago):-
    not(statusSangre(Mago, impura)).

condicionSly(Casa, _):-
    Casa \= slytherin.

% Punto 2
cumpleCaracter(Casa, Mago):-
    casa(Casa),
    mago(Mago),
    forall(caracterCasa(Casa, C), magoEs(Mago, C)).


% Punto 3

casaPosible(hermione, gryffindor).

casaPosible(Mago, Casa) :-
    permiteEntrar(Casa, Mago),
    cumpleCaracter(Casa, Mago),
    not(casaOdiar(Mago, Casa)).

% Punto 4

cadenaDeAmistades([M1, M2]):-
    minimoCadena(M1, M2).

cadenaDeAmistades([ M1, M2 | MS]):-
    minimoCadena(M1, M2),
    cadenaDeAmistades([ M2 | MS]).

minimoCadena(M1, M2):-
    magoEs(M1, amistad),
    magoEs(M2, amistad),
    casaPosible(M1, Casa),
    casaPosible(M2, Casa).

% ------------- Parte 2
accionPosible(ganarPartidaAjedrez, 50).
accionPosible(salvarAmigos,50).
accionPosible(vencerVoldemort, 60).

accionPosible(andarDeNoche, -50).

accionPosible(irALugar(Lugar), Puntaje):-
    lugarProhibido(Lugar, Puntaje).

accionPosible(irALugar(Lugar), 0):-
    not(lugarProhibido(Lugar, _)).

lugarProhibido(bosque, -50).
lugarProhibido(seccionRestringidaBiblioteca, -10).
lugarProhibido(tercerPiso, -75).

accion(harry, andarDeNoche).
accion(harry, irALugar(bosque)).
accion(harry, irALugar(tercerPiso)).
accion(harry, vencerVoldemort).
accion(hermione, irALugar(tercerPiso)).
accion(hermione, irALugar(seccionRestringidaBiblioteca)).
accion(hermione, salvarAmigos).
accion(draco, irALugar(mazmorras)).
accion(ron, ganarPartidaAjedrez).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% Punto 1A
buenAlumno(Mago):-
    mago(Mago),
    accion(Mago, _),
    not((accion(Mago, Accion), malaAccion(Accion))).

malaAccion(Accion):-
    accionPosible(Accion, Punto),
    Punto < 0.

% Punto 1B
accionRecurrente(Accion):-
    accion(Mago1, Accion),
    accion(Mago2, Accion),
    Mago1 \= Mago2.


% Punto 2
puntajeTotalCasa(Casa, Puntaje):-
    casa(Casa),
    findall(PuntajeAlumno, 
        puntajeAlumno(Casa, PuntajeAlumno), 
        PuntajesAlumnos),
    sumlist(PuntajesAlumnos, Puntaje).

puntajeAlumno(Casa, PuntajeMago):-
    esDe(Mago, Casa),
    findall(Puntaje, 
        (accion(Mago, Accion), accionPosible(Accion, Puntaje)),
        Puntajes),
    sumlist(Puntajes, PuntajeMago).

% Punto 3
casaGanadora(Ganadora):-
    puntajeTotalCasa(Ganadora, Max),
    forall(casa(Casa), 
        (puntajeTotalCasa(Casa, OtroPunto), 
        OtroPunto =< Max)).


% Punto 4

accionPosible(pregunta(_, Dif, snape), Valor):-
    Valor is Dif/2.

accionPosible(pregunta(_, Dificultad, Prof), Dificultad):-
    Prof \= snape.

accion(hermione, pregunta("¿Dónde se encuentra un Bezoar?", 20, snape)).
accion(hermione, pregunta("¿Cómo levitar una pluma?", 25, flitwick)).

  








