==Function and procedure test

a : int;
pa : °int := §a;
ppa : °°int := §pa;

array : Array <int> := [1,2,3];

proc main () is
 f();

 parray : °Array <int> := §array;
 x : int := array[1];
 y : int := (°parray)[++a];

 proc f () is
   y : °°int := §°ppa;
   x : int := °°ppa;
   °°ppa := x;
 end f

 a : real;
 a := °pa;
end main

proc f () is
  array : Array <Array <real>> := [[1,2,3],
                                  array,
                                  create Array <int> (3)];
  x : real := array[1][3];

  return;

  x := f(array[1]);

  func f (x : ref Array<real>(3)) -> (r : real) is
    r := 2 + 20*2;
    const n : int := 7;
    return;
    r := array[1][n % 2];
  end f
end f

const x : int := 3 + 4;
const y : real := -(3.0 + 4);
const z : real := 3.0 * x + 11;
m : real := 7 / 2;

proc g () is
  const a : int := x * 5;
  barray : Array<bool> := create Array<bool> (6*x % 2 + 1);
  sarray : Array<string> := create Array<string> (a - 4*x -2);
  if (sarray[1] = "") then
    barray[x-x] := sarray[0] <> "";
  end if

  proc manin() is
    s : string := "ciao";
    ps : °string := §s;

    return;

    func fval (in : string) -> (s : string) is
      in := "temporary";
      s := "mondo";
    end fval

    func fvalres (in : valres string) -> (s : string) is
      in := "overwritten";
      const x : int := 8;
      if (x = 5) then return; end if
      s := "";
    end fvalres

    return;

    const n : int := x;

    func fres (in :  string) -> (s : string) is
      s := "mondo";
      return;
      in := "copy-overwritten";
    end fres

    func fref (in : ref string) -> (s : string) is
      s := "";
      in := "";
    end fref

    return;

    arrs : Array <string> := [s, fval(s)];
  end manin
end g