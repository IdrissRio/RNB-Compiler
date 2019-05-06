

proc main() is 

x : real := 1.2;
x1 : bool := x<(x+3^2+x%2);
x2 : int := 1;

if x1 then
 x2 :=x2 + 2^3%1;
 x1 : int := x2;
elseif not x1 then 
 x1 : real := 3.1*9;
 x2 := 1+23;
else x1 := x1 and x1 or x1;
end if

MEGAIF : int :=1010;  

if x1 then
 if x1 then
  if x1 then
   ifGuard1: int := 1;
  elseif x1 then
   loop 
    loopGuard1 : int := 1;
   end loop 
  else ifGuard2 : int := 1;
  end if 
 elseif x1 then 
  ifGuard3 : int := 1;
 end if
else
 loop 
  loopGuard2 : int := 1;
  exiton x1;  
 end loop 
end if 

 MEGACASE : int := 01010;

case x1 of 

True -> 
 case x of 

 1.1 ->
  if x1 then 
   case x2 of 

   1 -> x1 : int := 1;
   2 -> x2 : int := 1;
   3 -> caseGuard1 : int := 1;
   
   else caseGuard2 : int := 1;
   end case 
  else x1 := True;
  end if 
 1.2 -> caseGuard3 : int := 1;
 end case
False -> 
 loop 
  case x1 of

  True -> continue;
  False -> exit;
  else exiton not x1;
  end case 
 end loop 
end case  


end main

