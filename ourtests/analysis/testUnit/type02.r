==Pointer declaration must be also definition. (line: 2, column: 1) [DeclError].
k : °int;
==Use of undeclared identifier 'k'. (line: 4, column: 13) [DeclError].
l : °int := k;
m : °°int := l;
==Incompatible conversion from '°int' to '°°int'. (line: 5, column: 11) [TypeError].
n : °bool := l; 
==Incompatible conversion from '°int' to '°bool'. (line: 7, column: 11) [TypeError].
==Use of undeclared identifier 'o'. (line: 10, column: 14) [DeclError].
o : °real := o; 
==Use of undeclared identifier 'q'. (line: 12, column: 26) [DeclError].
r : Array<Array<int>> := q[1];
==Use of undeclared identifier 'q'. (line: 14, column: 25) [DeclError].
s : Array<Array<int>> :=q;
==Symbol name 's' for variable declaration already used.  (line: 17, column: 1) [DeclError].
==Use of undeclared identifier 'q'. (line: 17, column: 25) [DeclError].
s : Array<Array<int>> :=q;
==Use of undeclared identifier 'q'. (line: 19, column: 26) [DeclError].
t : Array<Array<bool>> :=q;
==Array dimension must be an integer value in array initialization. (line: 21, column: 38) [TypeError].
u : Array<int> := create Array<int> (8.21);
==Array size requires a positive integer. (line: 23, column: 39) [TypeError].
v : Array<int> := create Array<int> (-10);
==Use of undeclared identifier 'j3'. (line: 25, column: 38) [DeclError].
v3: Array<int> := create Array<int> (j3);
==Use of undeclared identifier 'j5'. (line: 27, column: 40) [DeclError].
v4: Array<int> := create Array<int> (--j5);
==Use of undeclared identifier 'j6'. (line: 30, column: 40) [DeclError].

v5: Array<int> := create Array<int> (--j6);




v7: °Array<int> := v;
==Incompatible conversion from 'Array <int>(?)' to '°Array <int>(?)'. (line: 35, column: 17) [TypeError].
==Incompatible conversion from '°Array <int>(?)' to '°int'. (line: 40, column: 11) [TypeError].
v8: °Array<int> := §v;
v9: °int := §v3[1];
v10: °int := §v3;
==Use of undeclared identifier 'e1'. (line: 42, column: 23) [DeclError].
v11: Array<°int> := [§e1];

v12: °Array<°int> := §v11;
v14 : int := °(°v12)[1];
const v15 : Array<int> := [1,2,3,4];
v16 : Array<int> := v15;

v17 : Array<Array<int>> := [v15];

==Expected a basic type but found '°int'.          E.g., 'int', 'real', 'string', 'char' and 'bool'. (line: 53, column: 17) [DeclError].
==The return variable 'z' may be never initialized. (line: 53, column: 17) [ControlFlowAnalysis].
func test() -> (z: °int) is

end test
==Use of undeclared identifier 'e1'. (line: 57, column: 14) [DeclError].
v6: °int := §e1;

==Declaration of 'main' not found.



