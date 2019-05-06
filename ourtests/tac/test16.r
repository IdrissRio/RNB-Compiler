proc main () is
aa : Array<Array<int>> := [[1,2,3],[1,2,4]];

const x: int := 'c';

y:int := '\t';
as : Array<string> := create Array<string>(3);
ssss : string;
a : Array<int> := aa[y];
a[y] := a[a['c'+1]] +1;

c : string := '\n';
c1 : string := 'c';

cc : char := 'a';
r : real := 0.1 + cc;
end main

func f (x : Array<int>('c')) -> (y : int) is
y := x[1];
y := '\n';

end f


proc g () is
 c : char := 'c';
 z : int := 5 if ('a' < c) else 6;
  x : int := 0;
 a : Array<Array<int>> := [[1,2],[3,4]];
 a1 : Array<int> := a[x] if x <> 0 else a[x+1];
 a[x] := [1,2];
 ff(a[x]);
end g


proc ff (aa : Array<int>(2)) is
x : int := 1;
ff : real := aa[x] + aa[1];
end ff