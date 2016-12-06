:-dynamic pacman/3. 
:-dynamic ghost/4. 
:-dynamic powerBall/2.  
:-dynamic ghostPrev/3.

grid([[1,1,1,3,1,1,1], 
      [1,0,0,0,0,0,1], 
      [1,0,1,1,1,0,1], 
      [2,0,0,0,0,0,2],
      [1,0,1,0,1,0,1],
      [1,0,0,0,0,0,1],
      [1,1,1,3,1,1,1]]).

pacman(2,2,normal).
ghost(4,6,red,scatter).
ghostPrev(4,7,red).
%% ghost(6,6,pink,scatter).
%% ghostPrev(6,7,pink).
%% ghost(6,6,blue,scatter).
%% ghostPrev(6,7,blue).
%% ghost(6,6,orange,scatter).
%% ghostPrev(6,7,orange).

