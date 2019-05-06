const a : Array<int> := [1,2,3,4];
const a1 : int := 1;
a2 : int := 2;
const a3 : °int := §a2;
==You cannot assign the address of a constant variable to a variable. (line: 6, column: 7) [TypeError].
const a4 : °int := §a1;

proc main() is

a5 : int := °a4;
==You cannot assign the address of a constant variable to a variable. (line: 12, column: 1) [TypeError].
a6 : °int := a4;

a7 : Array<int> := a;
a8 : int := a[1];
a9 : Array<Array<int>> := [a];
a10 : Array<Array<int>> := [a7];
a10 := a9;
°a4:= 10;
const a11 : °int := a4;
const a12 : °int := a6;
const a13 : int := 1;
a14 : int := a13;


test(a5);

test(°a4);
==Cannot pass a const variable if the formal parameter is  not 'const' qualified.         The 1° parameter of the function 'test' is not const qualified. (line: 30, column: 6) [TypeError].
test(a13);
==Cannot pass a const variable if the formal parameter is  not 'const' qualified.         The 1° parameter of the function 'test' is not const qualified. (line: 32, column: 6) [TypeError].
test(a13);
==Undefined operator '+ : string x int -> τ'.         Maybe you want to use '+: τ_1 x τ_2 -> τ3'.         Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.         More details can be found in the documentation. (line: 34, column: 24) [TypeError].
a15 : string := "ciao" +1;

a5 := test1(°a11);
a5 := test1(a13);
==Passing the address of a const qualified variable. (line: 39, column: 13) [TypeError].
a5 := test2(a11);
==Passing the address of a const qualified variable. (line: 41, column: 14) [TypeError].
a5 := test2(§a13);
==Cannot change the value of the const variable 'a13'. (line: 44, column: 11) [TypeError].

a5 := a[++a13];
a5 := a[++a5];

end main


proc test(x : ref int ) is

end test


func test1(x : const int) -> (z:int) is
	z := 1;
end test1

func test2(x : const °int) -> (z:int) is
	arrInt : Array<int> := [1,2,3,4,5,6];
==Incompatible conversion from 'Array <Array <int>(6)>(2)' to 'Array <int>(2)'. (line: 62, column: 32) [TypeError].
	arrArrInt : Array<int> := [arrInt, [1,2,3,4,5,6]];
	z:=1;




end test2






