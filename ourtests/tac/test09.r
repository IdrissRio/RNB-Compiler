proc main() is
  const y : int := 5;
  a : Array <int> := [1,2,3,4,5,6];
  x : int := a[y];
  z : int := 1;
  x := a[z];
end main

proc second() is
  const y : int := 2;
  a : Array <Array<int>> := [[1,2,3],[4,5,6],[7,8,9]];
  x : int := a[y][1];
  z : int := 1;
  x := a[1][z];
end second

proc third () is
  y : int;
  a : Array < Array <int> > := [[1,2,3],create Array <int> (3), [7,8,9]];
  const x : int := a[y++][++y];
end third

const x : int := 4.9*9;

const b : bool := 4*7 > 6.5;