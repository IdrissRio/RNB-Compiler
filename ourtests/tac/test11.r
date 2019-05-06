proc main () is
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
        endloop : int := 1999;
    6 -> ycase6 : int;
  end case
end main