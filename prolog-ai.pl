%dynamic rule
:-use_module(library(lists)). 
:-dynamic height/1. 
:-dynamic width/1.


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
	\+reset,
	construct,
	grid([Row|Rest]),
	length([Row|Rest], H),
	retract(height(_)),		%del rule
	assert(height(H)),      %add rule
	length(Row,W),
	retract(width(_)),
	assert(width(W)).

width(0).
height(0).

%find the element in X and Y position Param(X,Y,value of element in that position)
element(X,Y,Z):- 
	grid(G), 
	nth1(Y,G,Line), 
	nth1(X,Line,Z). 

%pacman logic what is element on the pacman block.
validatePos(X,Y):-
  	element(X,Y,1), !, fail.

validatePos(X,Y):-
	powerBall(X,Y),
	retract(powerBall(X,Y)),
	retract(pacman(Px,Py,_)),
	assert(pacman(X,Y,beast)),
	changeAllGhostsMode(scare).

validatePos(X,Y).



%wrap portal logic
wrap(X,Y,NX,NY):-
	element(X,Y,Z),
	Z > 1,
	element(NX,NY,Z),
	\+ (NX== X, NY == Y), !.

getPacman(X,Y):-
	pacman(X,Y,_).

getGhost(Type,X,Y,Mode):-
	ghost(X,Y,Type,Mode).

%move pacman to new position
movePacman(X,Y):-
  	\+ghost(X,Y,_,scare),
  	pacman(_,_,normal),
  	fail.


movePacman(X,Y):-
	ghost(X,Y,T,scare),
	validatePos(X,Y),
	retract(pacman(_,_,Type)),
	assert(pacman(X,Y,Type)),
	createGhost.

movePacman(X,Y):- 
	wrap(X,Y,NX,NY),
	validatePos(NX,NY),
	retract(pacman(_,_,Type)),
	assert(pacman(NX,NY,Type)).

movePacman(X,Y):-
	validatePos(X,Y),
	retract(pacman(_,_,Type)),
  	assert(pacman(X,Y,Type)).

changeGhostMode(Type,Mode):-
	retract(ghost(X,Y,Type,_)),
	assert(ghost(X,Y,Type,Mode)).

changeAllGhostsMode(Mode):-
	retract(ghost(X1,Y1,red,_)),
	retract(ghost(X2,Y2,blue,_)),
	retract(ghost(X3,Y3,pink,_)),
	retract(ghost(X4,Y4,orange,_)),
	assert(ghost(X1,Y1,red,Mode)),
	assert(ghost(X2,Y2,blue,Mode)),
	assert(ghost(X3,Y3,pink,Mode)),
	assert(ghost(X4,Y4,orange,Mode)).

moveRedGhost:-
	pacman(Px,Py,_),
	moveGhost(red,(Px,Py)).

moveBlueGhost(X,Y):-
	moveGhost(blue,(X,Y)).

movePinkGhost(X,Y):-
	moveGhost(pink,(X,Y)).

moveOrangeGhost:-
	moveGhost(orange,G).


%for chase mode
%red ghost target
moveGhost(Type,Goal):-
	Type == red,
	ghost(X,Y, red, chase),
	targetGhost((X,Y),(NewX,NewY),Goal,Type),!,
	moveGhost(NewX,NewY,red,chase).

%orange ghost
moveGhost(Type,Goal):-
	Type == orange,
	ghost(X,Y, Type, chase),
	orangeGhost((X,Y),(NewX,NewY)),!,
	moveGhost(NewX,NewY,Type,chase).

%blue and pink ghost   
moveGhost(Type,Goal):-
	ghost(X,Y, Type, chase),
	targetGhost((X,Y),(NewX,NewY),Goal,Type),!,
	moveGhost(NewX,NewY,Type,chase).

%for scatter mode
moveGhost(Type,Goal):-
	ghost(X,Y,Type,scatter),write("hey"),
	scatterGhost((X,Y),(NX,NY),Type),!,
	moveGhost(NX,NY,Type,scatter).

%for scare mode when pacman eat power ball
moveGhost(Type,Goal):-
	ghost(X,Y,Type,scare),write("HI"),
	scatterGhost((X,Y),(NX,NY),Type),!,
	moveGhost(NX,NY,Type,scare).

%move ghost to new position
moveGhost(X,Y,Type,Mode):-
	wrap(X,Y,NX,NY),
  	retract(ghost(_,_,Type,Mode)),
  	retract(ghostPrev(_,_,Type)),
  	assert(ghost(NX,NY,Type,Mode)),
  	assert(ghostPrev(X,Y,Type)).

moveGhost(X,Y,Type,Mode):-
	retract(ghost(_,_,Type,Mode)),
	retract(ghostPrev(_,_,Type)),
	write(X), write("/"),write(Y),nl,
	assert(ghost(X,Y,Type,Mode)),
	assert(ghostPrev(X,Y,Type)).


%ghost target X point. param(currentPosition,nextposition,goalposition,typeofghost).
targetGhost(CurrPoint,(NextX,NextY),Goal,Type):-
	ghostPrev(Xprev,Yprev,Type),
	astar(CurrPoint,Goal,[(Xprev,Yprev)],[(X1,Y1),(NextX,NextY)|T],1,Temp,TotalCost).

%orange ghost behaviour param(CurrentPosition,outputPosition). 
orangeGhost(CurrPoint,NextMove):-
	pacman(Px,Py,_),
	h(CurrPoint,(Px,Py),D),
	D < 8,
	scatterGhost(CurrPoint,NextMove,orange).

orangeGhost(CurrPoint,NextMove):-
	pacman(Px,Py,_),
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
removeElement(N,[H|T],Z):- N == H, removeElement(N,T,Z).
removeElement(N,[H|T],[H|Z]):- removeElement(N,T,Z).  

% a star param(currentPosition,GoalPosition,VisitedPoint, outputPath, totalG, outputCost)
astar((X,Y),(X,Y),Visited,[(X,Y)],GValue,Temp,Temp):- !.
astar((X,Y),Goal,Visited,[(X,Y)],GValue,Temp,Temp):- GValue >  8,!.
astar((X,Y),Goal,Visited,[(X,Y)|T],GValue,Temp,TotalCost):-
	findAdj((X,Y),Visited,PossiblePoint),
	%% write("P: "),write(PossiblePoint),nl,
	recur(PossiblePoint,Goal,H,NextPoint),
	removeElement(NextPoint,PossiblePoint,NV),
	append([(X,Y)|Visited],NV,NewVisit),
	%% write("N: "),write(NextPoint),nl,write("V: "),write(NewVisit),nl,
	TotalCost1 is GValue + H,
	%% write("cost: "),write(GValue),nl,
	NewG is GValue + 1,
	astar(NextPoint,Goal,NewVisit,T,NewG,TotalCost1,TotalCost). 

%a star, but for the case that two blocks have the same cost value. param(2possibleMove,GoalPosition, VisitedPoint, outputPath, totalG, outputCost)
astar([First,Second|Tail],Goal,Visited,Path1,GValue1,Temp,TotalCost1):-
	%% write("in 2   first:"),write(First),write("   second:  "),write(Second),nl,
	G2 is GValue1,
	astar(First,Goal,Visited,Path1,GValue1,Temp,TotalCost1),
	astar(Second,Goal,Visited,Path2,G2,Temp,TotalCost2),
	%% write(First),write("Total1: "),write(TotalCost1),write(" "),write(Second),write("     Total2: "),write(TotalCost2),nl,
	TotalCost1 =< TotalCost2.

astar([First,Second|Tail],Goal,Visited,Path2,GValue1,Temp,TotalCost2):-
	%% write("in 2"),
	G2 is GValue1,
	astar(First,Goal,Visited,Path1,GValue1,Temp,TotalCost1),
	astar(Second,Goal,Visited,Path2,G2,Temp,TotalCost2),
	%% write(First),write("Total1: "),write(TotalCost1),write(Second),write("     Total2: "),write(TotalCost2),nl,
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
	assert(ghost(5,2,T,chase)),
	fail. 
createGhost.



