const arr : Array <int> := [1,2,3];
x : bool := "prova" = "prova";
==Cannot change the value of the const variable 'arr'. (line: 4, column: 14) [TypeError].
y : int := ++arr[1];

arr2 : Array <int> := [1,2,3];

const z:int :=1;

proc f( x :const °int) is

    °x:=1;
end f

proc main () is
==Cannot change the value of the const variable 'z'. (line: 17, column: 22) [TypeError].
	xxx :int :=++z++;
==Incompatible conversion from 'Array <int>(13)' to 'Array <int>(3)'. (line: 20, column: 12) [TypeError].
==Cannot change the value of the const variable 'arr'. (line: 20, column: 9) [TypeError].
	arr:= create Array <int> (13);
	const str : string := "prova";
==Cannot change the value of the const variable 'str'. (line: 23, column: 9) [TypeError].
	str := "prova";
	const int1 : int := 2+4+6+z+22;
==Incompatible conversion from 'real' to 'int'. (line: 26, column: 26) [TypeError].
	const int2 : int := 1.5;
	const int3 : int := int2;
==You cannot assign the address of a constant variable to a variable. (line: 29, column: 9) [TypeError].
	int4 : °int := §int3;
	int5 : °int := int4;
==Cannot change the value of the const variable 'int2'. (line: 32, column: 23) [TypeError].
	int6 : int := int2++;
end main

==Expected a basic type but found '°int'.          E.g., 'int', 'real', 'string', 'char' and 'bool'. (line: 36, column: 30) [DeclError].
func f2( x :const °int) -> ( y: °int )is

    °x:=1;
==You cannot assign the address of a constant variable to a variable. (line: 40, column: 5) [TypeError].
    y:=x;

   gg : Array<int> := [1,2,3,4];
   proc main() is 
 		x : int := 1;

 		y : int := gg[x];

 		z : °int := §x;
==Operator [] requires an Array. (line: 50, column: 26) [TypeError].
 		gg[1] := z[1];

 		const xx : int := 1;
		const kk : °int := §y;
==You cannot assign the address of a constant variable to a variable. (line: 55, column: 41) [TypeError].
 		g : Array<°int> := [kk,§xx];
		yy :int := °kk;
==You cannot assign the address of a constant variable to a variable. (line: 58, column: 17) [TypeError].
		z:=§xx;
		yy:=°kk;
 	end main
end f2


proc test(x: ref int) is
==The 1° parameter of the function 'test' requires a Left Expression. (line: 66, column: 14) [TypeError].
	test(2);
	y: int := 2;
	test(y);

	if True then
==Expression result unused. (line: 72, column: 17) [ControlFlowAnalysis].
		1=1;

	elseif True then
=='exit' statement not in a loop statement. (line: 76, column: 17) [ContextError].
		exit;
=='continue' statement not in a loop statement. (line: 78, column: 17) [ContextError].
		continue;
=='exiton' statement not in a loop statement. (line: 80, column: 17) [ContextError].
		exiton True;

	end if

	loop 
		exit;
		loop
			exit;
			loop
				exit;
			end loop
		end loop
	end loop

	case 1 of
=='exit' statement not in a loop statement. (line: 96, column: 14) [ContextError].
	1 -> exit;

	end case

	x1:Array<int>(2) := [1,2];
	x16 : °Array<int>(2) := §x1;

	xxx : Array<real> := [1,2];
	x3 : °Array<real> := §xxx;

	x3 := §xxx;
	xxx := °x3;

	loop 
		if True then
			continue;
		else exiton 
			True;
		end if
	end loop

	loop 
==Cannot use the statement 'return' inside a deterministic 'for' loop.  (line: 120, column: 7) [ControlFlowAnalysis].
      for(x : int := 1 -> 1)
      return;
      end for
      return;
    end loop	

	test(y);
end test





