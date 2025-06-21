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
  ocupa(Pais, Jugador, _).

% 2 - 
cantidadPaises(Jugador, CantidadPaises):-
  jugador(Jugador), % generación
  findall(Pais, ocupa(Pais, Jugador, _), Paises),
  length(Paises, CantidadPaises).
   
% Usamos findall, predicado de Orden Superior !  

jugador(Jugador):- % Predicado generador, usando de base objetivo/2, donde tenemos una definición para cada Jugador
  objetivo(Jugador, _).

% jugador(rojo). por extensión

% 3 - 
ocupaContinente(Jugador, Continente):-
  jugador(Jugador),
  continente(Continente),
  forall( paisContinente(Pais, Continente), ocupa(Pais, Jugador, _) ). % V(x) : p(x) => q(x)

% V => V = V
% V => F = F
% F => _ = V

% 4 - 
leFaltaMucho(Jugador, Continente):-
  jugador(Jugador),
  continente(Continente),
  findall(Pais, (paisContinente(Pais,Continente), not(ocupa(Pais,Jugador,_))), Paises),
  length(Paises, Cantidad),
  Cantidad > 2.


% 5 - 
sonLimitrofes(Pais1, Pais2):-
  limitrofes(Paises),
  member(Pais1, Paises),
  member(Pais2, Paises),
  Pais1 \= Pais2.

