%dynamic rule
:-use_module(library(lists)). 
:-dynamic biscuit/2. 
:-dynamic height/1. 
:-dynamic width/1.
:-dynamic the_end/1.
:-dynamic win/1.
:-dynamic ww/3.

%up (currentPosition, possibleMove1, possibleMove1, output).
priority((OldX,OldY),(X,Y),(W,Z),(OldX,Ny)):- Ny is OldY - 1,write(X),write("F"),write(Y), X == OldX, Y == Ny.
priority((OldX,OldY),(X,Y),(W,Z),(OldX,Ny)):- Ny is OldY - 1,nl,write(W),write("S"),write(Z),nl, W == OldX, Z == Ny.
%left
priority((OldX,OldY),(X,Y),(W,Z),(Nx,OldY)):- Nx is OldX - 1,write("3"), X == Nx, Y == OldY.
priority((OldX,OldY),(X,Y),(W,Z),(Nx,OldY)):- Nx is OldX - 1,write("4"), W == Nx, Z == OldY.
%down  
priority((OldX,OldY),(X,Y),(W,Z),(OldX,Ny)):- Ny is OldY + 1,write("5"), X == OldX, Y == Ny. 
priority((OldX,OldY),(X,Y),(W,Z),(OldX,Ny)):- Ny is OldY + 1,write("6"), W == OldX, Z == Ny. 
%right  
priority((OldX,OldY),(X,Y),(W,Z),(Nx,OldY)):- Nx is OldX + 1,write("7"), X == Nx, Y == OldY.     
priority((OldX,OldY),(X,Y),(W,Z),(Nx,OldY)):- Nx is OldX + 1,write("8"), W == Nx, Z == OldY. 

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

%find the element in X and Y position Param(X,Y,value of element in that position)
element(X,Y,Z):- 
	grid(G), 
	nth1(Y,G,Line), 
	nth1(X,Line,Z). 

create_biscuits:- 
	element(X,Y,0), 
	assert(biscuit(X,Y)), 
	fail. 
create_biscuits.

%pacman logic what is element on the pacman block.
validate_pos(X,Y):-
  	element(X,Y,1), !, fail.

validate_pos(X,Y):-
	powerBall(X,Y),
	rectract(powerBall(X,Y)),
	retract(pacman(Px,Py,normal)),
	assert(pacman(Px,Py,beast)),
	retract(ghost(Rx,Ry,red,_)),
	assert(ghost(Rx,Ry,red,scare)).

validate_pos(X,Y):-
	biscuit(X,Y),
  	retract(biscuit(X,Y)).

validate_pos(X,Y).

%wrap portal logic
wrap(X,Y,NX,NY):-
	element(X,Y,Z),
	Z > 1,
	element(NX,NY,Z),
	\+ (NX== X, NY == Y), !.

%move pacman to new position
move_pacman(X,Y):-
  	\+ghost(X,Y,_,scare),
  	pacman(_,_,normal),
  	fail.

move_paceman(X,Y):-
	ghost(X,Y,T,scare),
	validate_pos(NX,NY),
	retract(pacman(_,_,Type)),
	assert(pacman(NX,NY,Type)),
	createGhost.

move_pacman(X,Y):- 
	wrap(X,Y,NX,NY),
	validate_pos(NX,NY),
	retract(pacman(_,_,Type)),
	assert(pacman(NX,NY,Type)).

move_pacman(X,Y):-
	validate_pos(X,Y),
	retract(pacman(_,_,Type)),
  	assert(pacman(X,Y,Type)).

%ghost move logic param(blue ghost goal position, pink ghost goal position)
moveGhost((X,Y),(W,Z)):-
	pacman(Px,Py,_),
	move_ghost(red,(Px,Py)).
	%% move_ghost(blue,(X,Y)),
	%% move_ghost(pink,(W,Z)),
	%% move_ghost(orange,(Px,Py)).

%for chase mode
%red ghost target
move_ghost(Type,Goal):-
	Type == red,
	ghost(X,Y, red, chase),
	targetGhost(X,Y,NewX,NewY,Goal,Type),
	move_ghost(NewX,NewY,red,chase).

%orange ghost
move_ghost(Type,Goal):-
	Type == orange,
	ghost(X,Y, Type, chase),
	orangeGhost(X,Y,NewX,NewY),
	move_ghost(NewX,NewY,Type,chase).

%blue and pink ghost   
move_ghost(Type,Goal):-
	ghost(X,Y, Type, chase),
	targetGhost(X,Y,NewX,NewY,Goal,Type),
	move_ghost(NewX,NewY,Type,chase).

%for scatter mode
move_ghost(Type,Goal):-
	\+ghost(_,_,Type,chase),
	ghost(X,Y,Type,scatter),write("hey"),
	scatterGhost((X,Y),(NX,NY),Type),
	move_ghost(NX,NY,Type,scatter).

%for scare mode when pacman eat power ball
move_ghost(Type,Goal):-
	ghost(X,Y,Type,scare),
	ghostScary(X,Y,NX,NY),
	move_ghost(NX,NY,Type,scare).

%move ghost to new position
move_ghost(X,Y,Type,Mode):-
	wrap(X,Y,NX,NY),
  	retract(ghost(_,_,Type,Mode)),
  	retract(ghostPrev(_,_,Type)),
  	assert(ghost(NX,NY,Type,Mode)),
  	assert(ghostPrev(X,Y,Type)).

move_ghost(X,Y,Type,Mode):-
	retract(ghost(_,_,Type,Mode)),
	retract(ghostPrev(_,_,Type)),
	write(X), write("|"),write(Y),
	assert(ghost(X,Y,Type,Mode)),
	assert(ghostPrev(X,Y,Type)).


%ghost target X point. param(currentPosition,nextposition,goalposition,typeofghost).
targetGhost(CurrPoint,(NextX,NextY),Goal,Type):-
	ghostPrev(Xprev,Yprev,Type),
	astar(CurrPoint,Goal,[(Xprev,Yprev)],[(X1,Y1),(NextX,NextY)|T],1,TotalCost).

%orange ghost behaviour param(CurrentPosition,outputPosition). 
orangeGhost(CurrPoint,NextMove):-
	paceman(Px,Py,_),
	h(CurrPoint,(Px,Py),D),
	D < 8,
	scatterGhost(CurrPoint,NextMove,orange).

orangeGhost(CurrPoint,NextMove):-
	paceman(Px,Py,_),
	h(CurrPoint,(Px,Py),D),
	D >= 8,
	targetGhost(CurrPoint,NextMove,(Px,Py),orange).

%scatter mode for each type of ghost param(currentPosition,outputPosition,Typeofghost)
scatterGhost(CurrPoint,NextMove,Type):-
	Type == red,
	turnBase(CurrPoint,(1,2),Type,NextMove).

scatterGhost(CurrPoint,NextMove,Type):-
	Type == blue,
	turnBase(CurrPoint,(6,1),Type,NextMove).

scatterGhost(CurrPoint,NextMove,Type):-
	Type == pink,
	turnBase(CurrPoint,(6,7),Type,NextMove).

scatterGhost(CurrPoint,NextMove,Type):-
	Type == orange,
	turnBase(CurrPoint,(2,7),Type,NextMove).
	

% get every adjust block that is not a wall param(startPoint, visitedPoint,output).
findAdj(StartPoint,[],PossibleMove):- 
	findall((X,Y),(adj(StartPoint,(X,Y)), \+element(X,Y,1)), PossibleMove).

% get every adjust block that is not a wall and not already visit.
findAdj(StartPoint,Visited,PossibleMove):- 
	findall((X,Y),(adj(StartPoint,(X,Y)), \+element(X,Y,1), \+member((X,Y),Visited)), PossibleMove).

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

%find distance param(Point1,Point2, output)
h((X,Y),(X2,Y2),D) :-
  	D is abs(X - X2) + abs(Y - Y2).

%param(Element you want to del,array,output)
removeElement(N,[],[]):- !.
removeElement(N,[H|T],Z):- N == [H], removeElement(N,T,Z).
removeElement(N,[H|T],[H|Z]):- removeElement(N,T,Z).  

% a star param(currentPosition,GoalPosition,VisitedPoint, outputPath, totalG, outputCost)
astar((X,Y),(X,Y),Visited,[(X,Y)],GValue,TotalCost):- !.
astar(StartPoint,Goal,Visited,[StartPoint|T],GValue,TotalCost):-
	findAdj(StartPoint,Visited,PossiblePoint),
	%% write("P: "),write(PossiblePoint),nl,
	recur(PossiblePoint,Goal,H,NextPoint),
	removeElement(NextPoint,PossiblePoint,NV),
	append([StartPoint|Visited],NV,NewVisit),
	%% write("N: "),write(NextPoint),nl,write("V: "),write(Visit),nl,
	TotalCost is GValue + H,
	%% write("cost: "),write(TotalCost),nl,
	NewG is GValue + 1,
	astar(NextPoint,Goal,NewVisit,T,NewG,TotalCost).

%a star, but for the case that two blocks have the same cost value. param(2possibleMove,GoalPosition, VisitedPoint, outputPath, totalG, outputCost)
astar([First,Second|Tail],Goal,Visited,Path1,GValue,TotalCost1):-
	write("in 2"),
	astar(First,Goal,Visited,Path1,GValue,TotalCost1),
	astar(Second,Goal,Visied,Path2,GValue,TotalCost2),
	TotalCost1 =< TotalCost2.

astar([First,Second|Tail],Goal,Visited,Path2,I,TotalCost2):-
	write("in 2"),
	astar(First,Goal,Visited,Path1,I,TotalCost1),
	astar(Second,Goal,Visited,Path2,I,TotalCost2),
	TotalCost2 < TotalCost1.

%compare h value param(possibleMove,GoalPosition,outputCost, outputPoint).
recur([P],Goal,H,P):- h(P,Goal,H).
recur([F|T],Goal,H,P):- recur(T,Goal,H,P), h(F,Goal,NewH), H < NewH. 
recur([F|T],Goal,NewH,F):- recur(T,Goal,H,P), h(F,Goal,NewH), H > NewH. 
recur([F|T],Goal,H,Z):- recur(T,Goal,H,P), h(F,Goal,NewH), H == NewH, append([F],[P],Z).

%calculate one step ahead take param(currentPosition,GoalPosition,Type of the ghost, Output).
turnBase((X,Y),Goal,Type,NextMove):-
	ghostPrev(PrevX,PrevY,Type),
	findAdj((X,Y),[PrevX,PrevY],AdjPoint),
	recur(AdjPoint,Goal,H,NewPoint),
	turnBaseHelp(NewPoint,(X,Y),NextMove).

%for case that 2 ways have equal distance param(2PossibleMove, currentPoint, output).
turnBaseHelp((X,Y),OldPoint,(X,Y)):- !.
turnBaseHelp([First,Second|T],OldPoint,NewPoint):-
	write(First),nl,write(Second),nl,priority(OldPoint,First,Second,NewPoint),!,write("new point:"),write(NewPoint),nl.

createGhost:- 
	pacman(X,Y,_),
	retract(ghost(X,Y,T,scare)),
	assert(ghost(X,Y,T,chase)),
	fail. 
createGhost.



