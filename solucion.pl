/* distintos paises */
paisContinente(argentina, americaDelSur).
paisContinente(bolivia, americaDelSur).
paisContinente(brasil, americaDelSur).
paisContinente(chile, americaDelSur).
paisContinente(ecuador, americaDelSur).
paisContinente(alemania, europa).
paisContinente(espania, europa).
paisContinente(francia, europa).
paisContinente(inglaterra, europa).
paisContinente(aral, asia).
paisContinente(china, asia).
paisContinente(gobi, asia).
paisContinente(india, asia).
paisContinente(iran, asia).

/*países importantes*/
paisImportante(argentina).
paisImportante(kamchatka).
paisImportante(alemania).

/*países limítrofes*/
limitrofes([argentina,brasil]).
limitrofes([bolivia,brasil]).
limitrofes([bolivia,argentina]).
limitrofes([argentina,chile]).
limitrofes([espania,francia]).
limitrofes([alemania,francia]).
limitrofes([nepal,india]).
limitrofes([china,india]).
limitrofes([nepal,china]).
limitrofes([afganistan,china]).
limitrofes([iran,afganistan]).

/*distribución en el tablero */
ocupa(argentina, azul, 4).
ocupa(bolivia, rojo, 1).
ocupa(brasil, verde, 4).
ocupa(chile, negro, 3).
ocupa(ecuador, rojo, 2).
ocupa(alemania, azul, 3).
ocupa(espania, azul, 1).
ocupa(francia, azul, 1).
ocupa(inglaterra, azul, 2). 
ocupa(aral, negro, 2).
ocupa(china, verde, 1).
ocupa(gobi, verde, 2).
ocupa(india, rojo, 3).
ocupa(iran, verde, 1).

/*continentes*/
continente(americaDelSur).
continente(europa).
continente(asia).

/*objetivos*/
objetivo(rojo, ocuparContinente(asia)).
objetivo(azul, ocuparPaises([argentina, bolivia, francia, inglaterra, china])).
objetivo(verde, destruirJugador(rojo)).
objetivo(negro, ocuparContinente(europa)).

% 1 - 
estaEnContinente(Jugador, Continente):-
  paisContinente(Pais, Continente),
  ocupa(Pais, Jugador).

% 2 - 
cantidadPaises(Jugador, CantidadPaises):-
  jugador(Jugador), % generación
  findall(Pais, ocupa(Pais, Jugador), Paises),
  length(Paises, CantidadPaises).
   
% Usamos findall, predicado de Orden Superior !  

jugador(Jugador):- % Predicado generador, usando de base objetivo/2, donde tenemos una definición para cada Jugador
  objetivo(Jugador, _).

% jugador(rojo). por extensión

% 3 - 
ocupaContinente(Jugador, Continente):-
  jugador(Jugador),
  continente(Continente),
  forall( paisContinente(Pais, Continente), ocupa(Pais, Jugador) ). % V(x) : p(x) => q(x)

% V => V = V
% V => F = F
% F => _ = V

% 4 - 
leFaltaMucho(Jugador, Continente):-
  jugador(Jugador),
  continente(Continente),
  findall(Pais, 
    (paisContinente(Pais,Continente), not(ocupa(Pais,Jugador))), 
    Paises),
  length(Paises, Cantidad),
  Cantidad > 2.

% 5 - 
sonLimitrofes(Pais1, Pais2):-
  limitrofes(Paises),
  member(Pais1, Paises),
  member(Pais2, Paises),
  Pais1 \= Pais2.

% 6 -
esGroso(Jugador):-
  jugador(Jugador),
  forall(paisImportante(Pais), ocupa(Pais, Jugador)).
  
esGroso(Jugador):-
  cantidadPaises(Jugador, CantidadPaises),
  CantidadPaises > 10.

esGroso(Jugador):-
  jugador(Jugador),
  findall(Ejercito, ocupa(_, Jugador, Ejercito), Ejercitos),
  sum_list(Ejercitos, TotalEjercitos),
  TotalEjercitos > 50.
  
% 7 -
/*
estaEnElHorno/1: un país está en el horno si todos sus países limítrofes están ocupados por el mismo jugador que no es el mismo que ocupa ese país.
*/
ocupa(Pais, Jugador):- ocupa(Pais, Jugador, _).

estaEnElHorno(Pais):-
  ocupa(Pais, Jugador),
  jugador(Enemigo),
  Jugador \= Enemigo,
  forall( sonLimitrofes(Pais, Limitrofe), ocupa(Limitrofe, Enemigo) ).

estaEnElHorno2(Pais):-
  ocupa(Pais, Jugador),
  jugador(Enemigo),
  Jugador \= Enemigo,
  not((sonLimitrofes(Pais, Limitrofe), % Anti-patrón 
      not(ocupa(Limitrofe, Enemigo)))).

% 8 -
esCaotico(Continente):-
  continente(Continente),
  findall(Jugador, estaEnContinente(Jugador, Continente), Jugadores),
  list_to_set(Jugadores, JugadoresSinRepetidos),
  length(JugadoresSinRepetidos, TotalJugadores),
  TotalJugadores > 3.

% 9 - 
%capoCannoniere/1: es el jugador que tiene ocupado más países.
capoCannoniere(Jugador):-
  cantidadPaises(Jugador, Maximo),
  not((cantidadPaises(_, Cantidad),
    Cantidad > Maximo)).
  
capoCannoniere2(Jugador):-
  cantidadPaises(Jugador, Maximo),
  forall(
    (cantidadPaises(OtroJugador, Cantidad), Jugador \= OtroJugador),
    Cantidad < Maximo).

% 10 - 
% ganadooor/1: un jugador es ganador si logro su objetivo 

ganadooor(Jugador):-
  objetivo(Jugador, Objetivo),
  cumpleObjetivo(Jugador, Objetivo).
/*
Esta solución es mejor porque:
- Escribimos objetivo 1 sola vez
- Mayor cohesión, tenemos un predicado general y otro que se ocupa del cumplemiento del objetivo
- Menor acoplamiento, si se agrega un nuevo objetivo, solo tengo que hacer una nueva versión de cumpleObjetivo/2, gandooor/1 no se entera
- Polimorfismo, ganadooor/1 funciona independientemente de las formas que tenga el objetivo, ese es un problema de cumpleObjetivo/2
*/
cumpleObjetivo(Jugador, ocuparContinente(Continente)):-
  ocupaContinente(Jugador, Continente).

cumpleObjetivo(Jugador, ocuparPaises(Paises)):-
  forall(member(Pais, Paises), ocupa(Pais, Jugador)).

cumpleObjetivo(_, destruirJugador(OtroJugador)):-
  not(ocupa(_, OtroJugador)).
/*
ganadooor(Jugador):-
  objetivo(Jugador, ocuparContinente(Continente)),
  ocupaContinente(Jugador, Continente).

ganadooor(Jugador):-
  objetivo(Jugador, ocuparPaises(Paises)),
  forall(member(Pais, Paises), ocupa(Pais, Jugador)).

ganadooor(Jugador):-
  objetivo(Jugador, destruirJugador(OtroJugador)),
  not(ocupa(_, OtroJugador)). % cantidadPaises(OtroJugador, 0).
*/
