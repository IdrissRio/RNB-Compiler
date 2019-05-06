a1 : bool  := True;
==Use of undeclared identifier 'x'. (line: 3, column: 15) [DeclError].
b1 : bool  := x;
==Incompatible conversion from 'string' to 'bool'. (line: 5, column: 12) [TypeError].
c1 : bool  := "prova";
==Incompatible conversion from 'bool' to 'int'. (line: 8, column: 11) [TypeError].
d1 : real := 3.5;
e1 : int  := a1; 
==Incompatible conversion from 'real' to 'int'. (line: 10, column: 11) [TypeError].
f1 : int  := d1;
==Incompatible conversion from 'real' to 'bool'. (line: 12, column: 11) [TypeError].
g1 : bool := 2.6;
==Incompatible conversion from 'int' to 'bool'. (line: 14, column: 11) [TypeError].
a : bool  := 1;
==Incompatible conversion from 'string' to 'char'. (line: 17, column: 11) [TypeError].
b : char  := 'a';
c : char  := "abc";
==Incompatible conversion from 'string' to 'char'. (line: 19, column: 11) [TypeError].
d : char  := "a";
==Incompatible conversion from 'int' to 'char'. (line: 21, column: 11) [TypeError].
e : char  := 1;
==Incompatible conversion from 'real' to 'char'. (line: 23, column: 11) [TypeError].
f : char  := 3.5;
==Incompatible conversion from 'bool' to 'char'. (line: 25, column: 11) [TypeError].
g : char  := True;

h : int  := (e1);
i : int  := (h);
j : int  := (f);

j2 : int := 1;
const j3 : int := 1;
j4 : int := 1;
const j5 : int := --(j4);
const j6 : int := 6;
==Array dimension must be an integer value in array initialization. (line: 37, column: 37) [TypeError].
j7: Array<int> := create Array<int>(6.5*6.5);

k : string := 'c';
l : string := '\n';
m : Array<int> := ['\n', 'c','\t'];
n : Array<Array<int>> := [create Array<int>(10+'a'), create Array<int>(9+'b')];
==All initialization list elements must have the same lenght. (line: 44, column: 26) [DeclError].
o : Array<Array<int>> := [create Array<int>(1+'a'), create Array<int>(9+'b')];

p: bool:= 'b'*2/4*3<1+2+'c'+100;
q: bool;
r: int := -'c';
s : Array<Array<int>> := [create Array<int>('c'), create Array<int>('d'-1)];


==Declaration of 'main' not found.

cccc: int := 0;
proc main() is
	d : real :=1/ cccc;
end main

