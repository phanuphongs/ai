%dynamic rule
:-use_module(library(lists)). 
:-dynamic biscuit/2. 
:-dynamic height/1. 
:-dynamic width/1.
:-dynamic the_end/1.
:-dynamic win/1.

start:-
	new_world('/Users/patricksuphalawut/Documents/Project/Third_year_1s/ai/init.pl').

restart:-
	reconsult('/Users/patricksuphalawut/Documents/Project/Third_year_1s/ai/prolog-ai.pl').
	start.

new_world(Map):-
	consult(Map),
	grid([Row|Rest]),
	length([Row|Rest], H),
	retract(height(_)),		%del rule
	assert(height(H)),      %add rule
	length(Row,W),
	retract(width(_)),
	assert(width(W)),
	create_biscuits.

width(0).
height(0).

%find the element in X and Y position
element(X,Y,Z):- 
  grid(G), 
  nth1(Y,G,Line), 
  nth1(X,Line,Z). 

create_biscuits:- 
  element(X,Y,0), 
  assert(biscuit(X,Y)), 
  fail. 
  create_biscuits.

validate_pos(X,Y):-
  element(X,Y,1), !, fail.

validate_pos(X,Y):-
  \+biscuit(X,Y). 

validate_pos(X,Y):-
  retract(biscuit(X,Y)).

move_pacman(X,Y):-
  ghost(X,Y,_),
  pacman(_,_,normal),
  fail.

wrap(X,Y,NX,NY):-
	element(X,Y,Z),
	Z > 1,
	element(NX,NY,Z),
	\+ (NX== X, NY == Y), !.

move_pacman(X,Y):- 
	wrap(X,Y,NX,NY),
	validate_pos(NX,NY),
	retract(pacman(_,_,T)),
	assert(pacman(NX,NY,T)).

move_pacmac(X,Y):-
	validate_pos(X,Y),
	retract(pacman(_,_,T)),
  	assert(pacman(X,Y,T)).

move_ghost:-
	ghost(X,Y, red),
	redGhost(X,Y,NX,NY),
	write(NX), write("|"),write(NY),
	move_ghost(NX,NY,red).

move_ghost(X,Y,T):-
	wrap(X,Y,NX,NY),
  	retract(ghost(_,_,T)),
  	assert(ghost(NX,NY,T)).

move_ghost(X,Y,Z):-
	retract(ghost(_,_,Z)),
	write(X), write("|"),write(Y),
	assert(ghost(X,Y,Z)).

redGhost(X,Y,NX,NY):-
	pacman(PX,PY,normal),
	astar((X,Y),(PX,PY),[],[(X1,Y1),(NX,NY)|T],1,C).


% get every adjust block that is not a wall.
adjs(P,PossibleMove):- 
	findall((X,Y),(adj(P,(X,Y)), \+element(X,Y,1)), PossibleMove),write(PossibleMove),nl.

% get every adjust block that is not a wall and not already visit.
findAdj(P,[],PossibleMove):- 
	findall((X,Y),(adj(P,(X,Y)), \+element(X,Y,1)), PossibleMove).

% get every adjust block that is not a wall.
findAdj(P,V,PossibleMove):- 
	findall((X,Y),(adj(P,(X,Y)), \+element(X,Y,1), \+member((X,Y),V)), PossibleMove).

adj((X,Y),(X,NY)) :-
	height(H),
	Y < H,
	NY is Y + 1.

adj((X,Y),(X,NY)) :-
    Y > 1,
    NY is Y - 1.
  
adj((X,Y),(NX,Y)) :-
    width(W),
    X < W,
    NX is X + 1.
  
adj((X,Y),(NX,Y)) :- 
	Y > 1,
	NX is X - 1.

%find distance 
h((X,Y),(X2,Y2),D) :-
  	D is abs(X - X2) + abs(Y - Y2).

removeElement(N,[],[]):- !.
removeElement(N,[H|T],Z):- N == [H], removeElement(N,T,Z).
removeElement(N,[H|T],[H|Z]):- removeElement(N,T,Z).  



% a star 
astar((X,Y),(X,Y),V,[(X,Y)],I,C):- !.
astar((X,Y),G,V,[(X,Y)|T],I,C):-
	findAdj((X,Y),V,P),
	write("P: "),write(P),nl,
	recur(P,G,H,N),
	removeElement(N,P,NV),
	append([(X,Y)|V],NV,Visit),
	write("N: "),write(N),nl,write("V: "),write(Visit),nl,
	C is I + H,write("cost: "),write(C),nl,
	astar(N,G,Visit,T,I+1,C).

%a star, but for the case that two blocks have the same h value.
astar([(X,Y),(W,Z)|T],G,V,P1,I,C1):-
	write("in 2"),
	astar((X,Y),G,V,P1,I,C1),
	astar((W,Z),G,V,P2,I,C2),
	C1 =< C2.

astar([(X,Y),(W,Z)|T],G,V,P2,I,C2):-
	write("in 2"),
	astar((X,Y),G,V,P1,I,C1),
	astar((W,Z),G,V,P2,I,C2),
	C2 < C1.

%compare h value.
recur([P],G,H,P):- h(P,G,H).
recur([F|T],G,H,P):- recur(T,G,H,P), h(F,G,NH), H < NH. 
recur([F|T],G,NH,F):- recur(T,G,H,P), h(F,G,NH), H > NH. 
recur([F|T],G,H,Z):- recur(T,G,H,P), h(F,G,NH), H == NH, append([F],[P],Z).


	



