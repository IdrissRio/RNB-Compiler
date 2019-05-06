const INTN : int := 8;
REALN : real := INTN;

const REAL : real := INTN + REALN;

proc m() is

array : Array <real> := [1.0, 1.2, 1.3];
parray : °Array <real> := §array;

foreach (i : real -> °parray)
  x : real := i;
end foreach

const OLE : int := 8;

iarray : Array <int> := [1,2,3];
iiarray : Array <Array <int >> := [iarray, iarray, iarray];

foreach (x : Array<int> -> iiarray)
  y : int := x[1];
end foreach

end m

proc main() is

for (i : int := 1 -> 10)
  culonudo : int;
end for

y : string;

loop
for (i : int := 1 -> 10)
  x : int;
  return;
  exiton (x = 5);
  x := 7;
  continue;
end for
x : real;
continue;
x := 0.9;
end loop

exitvar : real;

aaaa : Array<Array<int>> := [[1,2,3],[1,2,3]];
foreach (x : int -> aaaa[1])
  elem : int := x;
end foreach

end main