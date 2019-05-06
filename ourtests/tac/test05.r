proc main () is
  x : int := 6;
  y : int := ++x;
end main


const x : int := 3 + 4;
const y : real := -(3.0 + 4.0);
w : int := 9 + 1;
const z : real := 3.0 * w + x;

proc f() is
  a : int := x * 5;
  array : Array<bool> := create Array<bool> (7*x - 30);
end f


proc g () is
  x : Array <Array <int >> := [[1,2,3],[4,5,6]];
  y : int := x[1][1];
  y := x[1][1];
end g