==All initialization list elements must have the same lenght. (line: 2, column: 30) [DeclError].
arr1: Array <Array <int>> := [[1], [2,3,4], create Array<int>(3)];
proc main() is
	case 2 of
==Expression result unused. (line: 6, column: 13) [ControlFlowAnalysis].
	2-> 2+2;
==Non compatible types in 'case' statement. Expected 'int' but found 'string'. (line: 9, column: 9) [TypeError].
==Expression result unused. (line: 9, column: 19) [ControlFlowAnalysis].
	"prova"-> 2;
	else 
	x : int := 2;
	end case
==Use of undeclared identifier 'x'. (line: 14, column: 9) [DeclError].
	x:=2;
==Illegal expression of type 'Array <int>(3)' inside the statement 'case'.         The operator '= :Array <int>(3) x Array <int>(3) -> bool' is not defined. (line: 16, column: 15) [TypeError].
	case [1,2,3]  of
==Non compatible types in 'case' statement. Expected 'Array <int>(3)' but found 'Array <int>(2)'. (line: 19, column: 10) [TypeError].
=='exit' statement not in a loop statement. (line: 19, column: 18) [ContextError].
	[1,2] -> exit;
	end case

==Empty 'case' statement. (line: 23, column: 9) [ControlFlowAnalysis].
	case "prova" of
	end case
==Undefined operator '^ : int x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 26, column: 15) [TypeError].
	case 2^2.5 of
	2-> 
	end case

	case 2 of
==Undefined operator '^ : int x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 32, column: 10) [TypeError].
	2^2.5-> 
	end case

	case True of
==Non compatible types in 'case' statement. Expected 'bool' but found 'int'. (line: 37, column: 9) [TypeError].
	1  ->v : real := 1.9;
	end case
	globalMain : int :=19;
	case True of
==Undefined operator '+ : int x string -> τ'.         Maybe you want to use '+: τ_1 x τ_2 -> τ3'.         Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.         More details can be found in the documentation. (line: 42, column: 12) [TypeError].
	1+1+"string" -> v: real:=1.9;
==Symbol name 'v' for variable declaration already used.  (line: 44, column: 9) [DeclError].
	v:real:=19.9;

	True -> v:real :=19.9;
	globalMain := 0;
	else
	globalMain := 0;
	v:real:=19.0;
	end case

end main