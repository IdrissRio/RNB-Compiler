==IMPY tests

const PP : int := 1;
const KK : int := 3+PP;

w : int := 5;
r : int := 2; 
u : Array<int> := [w*(KK+1-r),-w];
a : int := u[r];
mat : Array<Array<int>> :=[[1,2+KK],[3,4]];
pi : °int := §w;
x : real := 5+f(r);

proc main() is

  func f (k : ref int) -> (c : int ) is
    k++;
    c := c + ++k;
    return;
  end f 
  
  c : char  := 'r';
  
  u[w] := r+f(r);
  mat[r][a] := 0;
  
  if (c>PP) then mat[r][a]++; end if
  
  r := u[r++]++;
  
  u[++r] := u[++r] + u[r++];
  
  r := r + (°pi)++;
  
  for ( w: int  := 'a' -> 2) 
    u[r++] := w + r *°pi;
    r := u[w];
  end for  
  
  w := w++;
end main

func f ( k : valres int ) -> (c : int) is
  k++;
  c := ++k+2;
  return;
end f