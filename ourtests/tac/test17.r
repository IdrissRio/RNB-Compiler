proc f () is

try
x : int := 999;
try
 x := 0;
catch
 x : real;
end catch

afterfirstcatch : int;

catch
 y : real;
end catch
aa : Array<Array<int>> := [[1,2,4],[4,5,7]];
res : int := fff(aa[1]);
end f


func fff (a : ref Array<int>(3))->(z:int) is
x : int := 7;

y : int := x++ if (x=7) else ++x;
z:= a[1];
end fff