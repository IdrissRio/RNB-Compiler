==Generic test
proc main () is
  aa : Array<Array<int>> := [[1,2,3,4],[1,2,4,3]];

  const x: char := 'c';

  y:int := '\t';
  as : Array<string> := create Array<string>(3);
  ssss : string;
  a : Array<int> := aa[y];
  a[y] := a[a['\n'-8]] +1;

  try
    a1 : Array<int> :=  [1,2,3,4] if as[1] = "" else [4,3,2,1];
  catch 
    try
      a1 : Array<int> := aa[1] if x <> as[0] else aa[0];
      a1[1] := gVal(a1);
      c : string := '\n';
      c1 : string := 'c';
      cc : char := 'a';
      r : real := 0.1 + cc;

      a2 : int:= gValRes(a1);
      a3 : string := gRef(a1); 
    catch 

      case x of
      'a' -> as[1] := x;
      'c' -> as[0] := 'g';
      else as['\n'-10] := "x";
      end case

    end catch
  end catch
end main

func f (x : Array<int>('c')) -> (y : int) is
  y := x[1];
  y := '\n';

end f

proc fun () is
  dst : Array<int>(4) := [gVal([1,2,3,4]),2,3,4];
  in :  int := gValRes(dst);
  n : int := 1;
  dst[3-n] := in;
end fun

func gVal (x : Array<int>(4)) -> (r : int ) is
  r := x[1];
  x := create Array<int> (4);
end gVal

func gValRes (x : valres Array<int>(4)) -> (r : char) is
for(y : int := '\n' -> 20)
x[1] := r;
end for
end gValRes

func gRef (x : ref Array<int>(4)) -> (r : string ) is
try
a : Array<int> := x;
a1 : °Array<int> := §x;
r := "test";
catch 
r := "truIF" if x[1] <= x[2] else "falseIF";
end catch
end gRef