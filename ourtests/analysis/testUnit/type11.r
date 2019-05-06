
a : int := 1;
a1 : °int := §a;

x: Array<int> := [1];
x1:Array<int>(2) := [1,2];
==Incompatible conversion from 'Array <int>(1)' to 'Array <int>(2)'. (line: 8, column: 19) [TypeError].
x2:Array<int>(2)  := x;
x3:Array<Array<int>(2)>(2):= [x1,x1];
x4:Array<Array<int>(1)>(2):=[[1],create Array<int> (1)];
==Array dimension must be an integer value in array initialization. (line: 12, column: 15) [TypeError].
x5:Array<int>(1.2) := x;
==Array dimension must be an integer value in array initialization. (line: 14, column: 15) [TypeError].
x6:Array<int>(True) := x;
==Array dimension must be an integer value in array initialization. (line: 16, column: 15) [TypeError].
x7:Array<int>("ciao") := x;
x8:Array<int>:= x;
==Non homogeneous array initialization. (line: 19, column: 17) [TypeError].
x9:Array<real>:=[1,1.3,1];
x10:Array<int>:=[1,a,°a1,a+1];
==Array declaration must be also definition. (line: 22, column: 1) [DeclError].
x11:Array<int>(0);
==Initialization lists must have at least one element. (line: 25, column: 20) [DeclError].
==Array size requires a positive integer. (line: 25, column: 16) [TypeError].
x12:Array<int>(0):=[];
==Array size requires a positive integer. (line: 27, column: 16) [TypeError].
x13:Array<int>(0):=[1];


==Incompatible conversion from 'Array <int>(1)' to 'Array <int>(3)'. (line: 31, column: 21) [TypeError].
x14 : Array<int>(3) := [1];
x16 : °Array<int>(2) := §x1;
x17 : Array<int>(2) := x3[1];
==Incompatible conversion from 'Array <int>(1)' to 'Array <int>(2)'. (line: 35, column: 21) [TypeError].
x18 : Array<int>(2) := x4[1];
==Incompatible conversion from '°Array <int>(2)' to '°Array <int>(1)'. (line: 37, column: 21) [TypeError].
x19: °Array<int>(1) := §x1;
==All initialization list elements must have the same lenght. (line: 39, column: 22) [DeclError].
x20:Array<int>(3) := [x,1];
==Declaration of 'main' not found.



