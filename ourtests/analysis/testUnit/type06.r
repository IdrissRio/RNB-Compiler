proc main () is 
	const c :int := 2;
	pozzo   : int   := testVal(2);
	pointer : °int  := §pozzo;
	pozzo := testRef(pozzo);
==The 1° parameter of the function 'testRef' requires a Left Expression. (line: 7, column: 26) [TypeError].
	pozzo := testRef(2);

	pozzo := testRefP(§pozzo);
	
	pozzo := testConst(pozzo);

	

	pozzo := testValRes(pozzo);
	
	pozzo := testConst(2);


==The 1° parameter of the function 'testValRes' requires a Left Expression. (line: 21, column: 29) [TypeError].
	pozzo := testValRes(2);

	x : int;
==The 1° parameter of the function 'f' requires exactly the type 'real' but found 'int'. (line: 25, column: 11) [TypeError].
	f(x);

	y : real;

	f(y);

    
	b: real := 3.5;


	if b then
==Expression result unused. (line: 37, column: 17) [ControlFlowAnalysis].
		1=1;
==Expected a boolean expression but found 'real'. (line: 35, column: 12) [TypeError].
	elseif b then
==Expected a boolean expression but found 'real'. (line: 39, column: 16) [TypeError].
		exit;
=='exit' statement not in a loop statement. (line: 41, column: 17) [ContextError].
	end if

	case 1 of
=='exit' statement not in a loop statement. (line: 47, column: 22) [ContextError].
		1 -> exit;

	end case

end main
==The return variable 'z' may be never initialized. (line: 53, column: 27) [ControlFlowAnalysis].
func testVal(x :int ) -> (z: int) is
end testVal
==The return variable 'z' may be never initialized. (line: 56, column: 32) [ControlFlowAnalysis].
func testRef(x : ref int ) -> (z: int) is
end testRef
==The return variable 'z' may be never initialized. (line: 59, column: 34) [ControlFlowAnalysis].
func testRefP(x : ref °int ) -> (z: int) is
end testRefP
==The return variable 'z' may be never initialized. (line: 62, column: 36) [ControlFlowAnalysis].
func testConst(x : const int ) -> (z: int) is
end testConst



==The return variable 'z' may be never initialized. (line: 68, column: 38) [ControlFlowAnalysis].
func testValRes(x : valres int ) -> (z: int) is
end testValRes

proc f(a : ref real) is
end f

