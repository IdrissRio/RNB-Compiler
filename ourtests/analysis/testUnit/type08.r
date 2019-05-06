proc main() is
	pozzo : real := 2;
	pozzo := 2+2;
	pozzo := 2*2;
	pozzo := 2/2;
	pozzo := 2%2;
	pozzo := 2^2;
==Non homogeneous array initialization. (line: 9, column: 33) [TypeError].
	aReal : Array<real>  := [1.2,1, 1.3];
==Incompatible conversion from '°real' to '°int'. (line: 11, column: 27) [TypeError].
	const pInt : °int := §pozzo;
	const pReal : °real := §pozzo;
	pozzo := °pReal + °pReal;
	pozzo := °pReal + °pInt;
	pozzo := °pInt + °pReal;
	pozzo := 2 + °pReal;
	pozzo := °pReal + 2;
	pozzo :=  aReal[1]+aReal[2];
	pozzo := aReal[1] + 3 + pozzo + °pInt;

	pozzo := °pReal - °pReal;
	pozzo := °pReal - °pInt;
	pozzo := °pInt - °pReal;
	pozzo := 2 - °pReal;
	pozzo := °pReal - 2;
	pozzo :=  aReal[1]-aReal[2];
	pozzo := aReal[1] - 3 - pozzo - °pInt;

	pozzo := °pReal * °pReal;
	pozzo := °pReal * °pInt;
	pozzo := °pInt * °pReal;
	pozzo := 2 * °pReal;
	pozzo := °pReal * 2;
	pozzo :=  aReal[1]*aReal[2];
	pozzo := aReal[1] * 3 * pozzo * °pInt;

	pozzo := °pReal / °pReal;
	pozzo := °pReal /°pInt;
	pozzo := °pInt /°pReal;
	pozzo := 2 /°pReal;
	pozzo := °pReal /2;
	pozzo :=  aReal[1]/aReal[2];
	pozzo := aReal[1] /3 /pozzo / °pInt;

==Undefined operator '^ : real x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 46, column: 25) [TypeError].
	pozzo := °pReal ^ °pReal;
==Undefined operator '^ : real x int -> Int'.         Maybe you want to use '^: int x int -> int' (line: 48, column: 25) [TypeError].
	pozzo := °pReal ^°pInt;
==Undefined operator '^ : int x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 50, column: 24) [TypeError].
	pozzo := °pInt ^°pReal;
==Undefined operator '^ : int x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 52, column: 20) [TypeError].
	pozzo := 2 ^°pReal;
==Undefined operator '^ : real x int -> Int'.         Maybe you want to use '^: int x int -> int' (line: 54, column: 25) [TypeError].
	pozzo := °pReal ^2;
==Undefined operator '^ : real x real -> Int'.         Maybe you want to use '^: int x int -> int' (line: 56, column: 26) [TypeError].
	pozzo := aReal[1]^aReal[2];
==Undefined operator '^ : real x int -> Int'.         Maybe you want to use '^: int x int -> int' (line: 58, column: 27) [TypeError].
	pozzo := aReal[1] ^ 3 ^ pozzo ^ °pInt; 
==Undefined operator '+ : string x int -> τ'.         Maybe you want to use '+: τ_1 x τ_2 -> τ3'.         Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.         More details can be found in the documentation. (line: 60, column: 26) [TypeError].
	pozzo := "prova" + 2;
==Undefined operator '/ : string x int -> τ'.         Maybe you want to use '/: τ_1 x τ_2 -> real'.         Where τ_1, τ_2 ∈ {'int','real'}.         More details can be found in the documentation. (line: 62, column: 26) [TypeError].
	pozzo := "prova" / 2;
==Undefined operator '^ : string x int -> Int'.         Maybe you want to use '^: int x int -> int' (line: 64, column: 26) [TypeError].
	pozzo := "prova" ^ 2;







	 

end main