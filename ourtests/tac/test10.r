proc main () is

y : int := 7;

while (y < 10) do
 x : int := 5;
 if (x = 7) then thenvar:int; exit;
            else elsevar:int; continue;
 end if
 ++y;
end while

x:real;

end main


proc main2 () is

y : int := 7;

do
 y : int := 5;
 x : int := 6;
 if (x = 7) then thenvar:int; exit;
            else elsevar:int; continue;
 end if
 ++y;
until (y < 7)

x:real;

end main2


proc f() is


for ( i : int := 6 <- 10)
  inloop : real;
end for


end f

