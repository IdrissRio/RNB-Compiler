== Control flow and test:

const x : int := 5;

proc main () is

  initmain : int;

  loop
    beforeexiton : int;
    exiton (x = 5);
    afterexiton : int;
  end loop

  beforeloop : int;

  loop
   insideloop : int;
   proc f() is
     fvar : char;
     g();
     continue;
   end f

   exit;

   proc g() is
     f();
     proc f() is
       fgvar : string;
       g();
     end f
     gvar : string;
   end g

  end loop

  afterloop : int;

end main

const b : bool := True;
const b1 : bool := False;
b2 : bool := b <> b1;
b3 : bool := b = b2;

func test() -> (y : int) is
  const x : int := 6;
  y := 18;
  if (x < y) then
     thenvar : char;
     proc f () is
          fthenvar : int;
     end f
  elseif (3 < 6) then
    elseifvar : char;
    local
      return;
    end local
    f();
  else
     f();
     elsevar : int;
     proc f() is
          felsevar : int := x;
     end f
  end if

  func f() -> (result : int) is
       return;
       outerf : int;
       result  := outerf;
  end f
  const n : int := 5;

  proc test2 () is
    x : int := 6;
    y : int := 9;

    case x of
      5+4 -> ycase5 : int;
      7 -> ycase7 : int;
      8 -> ycase8 : int;
        loop
          y := 9;
          exiton (x = 8);
          y := 10;
          continue;
          y := 11;
        end loop
        endloopvar : int := 1994;
      6 -> ycase6 : int;
    end case
  end test2

end test