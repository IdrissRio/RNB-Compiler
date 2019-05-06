y : int := 5;
py : °int := §y;

proc whattafuck () is
 ++(°py)++;
end whattafuck

proc main () is
 b : bool := (3 >= 5);
 f(y);
end main

x : int := 4;

proc f (y : ref int) is
 b : bool := False and (x < 5);
 z : int := ++x;
 w : int := ++y;
end f


proc g () is
 b : bool := False;
 b1 : bool := (True and False) and b;
end g
