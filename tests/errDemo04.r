proc main() is

a : int := 1;
a1 : °int := §a;
a2 : real := 2.1;
a3 : °real := §a2;
==Pointer declaration must be also definition. (line: 11, column: 1) [DeclError].
°a3 := a;
a3 := a3; 

a4 : °real;
==Array declaration must be also definition. (line: 13, column: 1) [DeclError].
a5 : Array<real>;
a6 : real ;

a7 : string := "ciao";
a8 : °string := §a7;
==Non homogeneous array initialization. (line: 19, column: 20) [TypeError].
x : Array<real> := [1,2.3,°a1,a2,°a3];  
x1 : Array<real> :=  x;
x2 : Array<Array<real>> := [x,x1,create Array<real>(5)];
x1 := x2[1];
x2[1] := x1; 
x3 : Array<°Array<real>> := [§x1,§x,§x2[2],§x2[1]];
x4 : Array<Array<°real>> := [[a3,§a2],[§a2,a3]];
x5 : °Array<Array<real>> := §x2;
x6 : Array<°Array<°real>> := [§x4[1]];
==Initialization lists must have at least one element. (line: 29, column: 21) [DeclError].
x8 : Array<real> := []; 
==Dereferencing requires pointer operand. (line: 32, column: 9) [TypeError].
==Operator [] requires an Array. (line: 32, column: 9) [TypeError].
a2 := (°x2[1])[1];
a2 := °(°(x6[1]))[1];
==Incompatible conversion from 'Array <int>(2)' to 'Array <real>(5)'. (line: 35, column: 4) [TypeError].
x1 := [1,3]; 
x2 := °x5;
x3[2] := §x;
x4[2] := °x6[1];

s1 : string := a7;
==Non homogeneous array initialization. (line: 42, column: 23) [TypeError].
s2 : Array<string> := [s1,"mondo",§a8];
s3 : Array<Array<string>> := [s2];
s4 : °Array<string> := §s2;
s5 : Array<string> := [s1];
s6 : Array<string> := [s1,"hello","world", s1];

s2[3]:= s1;
s3[1][2] := "salve";
==Incompatible conversion from 'int' to 'string'. (line: 51, column: 10) [TypeError].
s3[1][2] := 1;
==Incompatible conversion from 'Array <string>(1)' to 'Array <string>(3)'. (line: 53, column: 7) [TypeError].
s3[0] := s5;
==Incompatible conversion from 'Array <string>(4)' to 'Array <string>(3)'. (line: 55, column: 7) [TypeError].
s3[0] := s6;
s3[1] := s2;
°s4 := s3[1];
s4 := §s3[2];

==Incompatible conversion from '°Array <string>(4)' to '°Array <string>(3)'. (line: 61, column: 4) [TypeError].
s4 := §s6;
==Dereferencing requires pointer operand. (line: 63, column: 11) [TypeError].
s3[0] := °s6;
==Use of undeclared identifier 'S4'. (line: 66, column: 11) [DeclError].
==Dereferencing requires pointer operand. (line: 66, column: 11) [TypeError].
s3[0] := °S4;
==Mismatch in argument types. (line: 68, column: 1) [TypeError].         In function call: 'test(Array <real>(5))'.         Function definition found at <l:84,c:6> with signature: 'test(Array <real>(?))'.
test(x);
test1(x2);
test2(x4);
test3(x3);
test4(x2);
test5(x3);
test6(x);

test8(x2);

test10(x3);


end main

==The array dimension for the parameter 'x' must be specified. (line: 84, column: 13) [TypeError].
proc test ( x : Array<real> ) is
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
==Use of undeclared identifier 'x2'. (line: 109, column: 1) [DeclError].
x2[1] := x [2];
end test4


proc test5 ( x :ref Array<°Array<real>(5)>(4) ) is
==Incompatible conversion from 'int' to '°Array <real>(5)'. (line: 115, column: 6) [TypeError].
x[2] := 1;
end test5



proc test6( x : valres Array<real>(5) ) is
x[2] := 1;
end test6


proc test8 ( x : valres Array<Array<real>(5)>(3) ) is
x[2][2] := 1;
x[2] := [1,2,3,4,5];
end test8


proc test10 ( x : valres Array<°Array<real>(5)>(4) ) is
(°x[2])[2] := 1;
°x[2] := [1,2,3,4,5];
end test10







