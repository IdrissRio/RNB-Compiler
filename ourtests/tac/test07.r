y : int := 10;

proc main () is
 func f() -> (result : int) is
   fmain : int;
   result := fmain;
  end f

  const x : int := 5;
  loop
    beforeexiton : real;
    exiton (x = 5);
    afterexiton : real;
  end loop

  beforeloop : int := 555;
  loop
  insideloop : int := 777;

  proc f() is
    fvar : char;
    g();
  end f

  exiton (True);

  proc g() is
    f();
    proc f() is
      fgvar : real;
      g();
    end f
    gvar : real;
  end g

  end loop
  afterloop : int := 999;

end main

