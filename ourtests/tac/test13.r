


proc main() is

a : int := 1;
a1 : °int := §a;
a2 : real := 2.1;
a3 : °real := §a2;

°a3 := a;
a3 := a3; 


a6 : real ;

a7 : string := "ciao";
a8 : °string := §a7;

x : Array<real> := [1.1,2.0,a2,a2,°a3];  
x1 : Array<real> :=  x;
x2 : Array<Array<real>> := [x,x1,create Array<real>(5)];
x1 := x2[1];
x2[1] := x1; 
x3 : Array<°Array<real>> := [§x1,§x,§x2[2],§x2[1]];
x4 : Array<Array<°real>> := [[a3,§a2],[§a2,a3]];
x5 : °Array<Array<real>> := §x2;
x6 : Array<°Array<°real>> := [§x4[1]];

a2 := (x2[1])[1];
a2 := °(°(x6[1]))[1];

x3[2] := §x;
x2 := °x5;
x4[2] := °x6[1];

s1 : string := a7;
s2 : Array<string> := [s1,"mondo",°a8];
s3 : Array<Array<string>> := [s2];
s4 : °Array<string> := §s2;

s2[3]:= s1;
s3[1][2] := "salve";
s3[2][2] := s1;
°s4 := s3[1];
s4 := §s3[2];


test(x);
test1(x2);
test2(x4);
test3(x3);
test4(x2);
test5(x3);

test6(x);
test7(x1);
test8(x2);
test9(x2);
test10(x3);
test11(x3);

end main


proc test ( x : Array<real>(5) ) is
x[1] := 1;
z : real := x[2];
end test 

proc test1 ( x : Array<Array<real>(5)>(3) ) is
x[2][2] := 1;
z : real := x[2][1];
z1: Array<real> := [z];
end test1 

proc test2 ( x : Array<Array<°real>(2)>(2) ) is
 °x[2][2] := 1;
 z : real := °x[2][2];
end test2 


proc test3 ( x : Array<°Array<real>(5)>(4) ) is
(°x[1])[1] := 1;
end test3


proc test4 ( x :ref Array<Array<real>(5)>(3)) is
x[2][2] := 1;
x[1] := x[2];
end test4


proc test5 ( x :ref Array<°Array<real>(5)>(4) ) is
(°x[2])[2] := 1;
end test5

proc test6( x : valres Array<real>(5) ) is
x[2] := 1;
end test6

proc test7 ( x : res Array<real>(5) ) is
x[2] := 1;
x1 : °real := §x[3];
end test7

proc test8 ( x : valres Array<Array<real>(5)>(3) ) is
x[2][2] := 1;
x1 : Array<int> := [1,2,3,4,5];
x[2] := x1;
end test8

proc test9 ( x : res Array<Array<real>(5)>(3) ) is
x[2][2] := 1;
x1 : Array<int> := [1,2,3,4,5];
x[2] := x1;
end test9

proc test10 ( x : valres Array<°Array<real>(5)>(4) ) is
(°x[2])[2] := 1;
x1 : Array<int> := [1,2,3,4,5];
°x[2] := x1;
end test10

proc test11 ( x : res Array<°Array<real>(5)>(4) ) is
(°x[2])[2] := 1;
x1 : Array<int> := [1,2,3,4,5];
°x[2] := x1;
end test11



