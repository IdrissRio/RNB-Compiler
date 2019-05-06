== For and try catch test

proc main() is

	const x : char := 'x';
	x1 : int := x;
	x2 : string := x;


	x3 : int := x1 +1 % 'c' ^ 2;
	x4 : Array<char> := ['a','b','a','\n','\\']; 
	x5 : Array<string> := x4;

	x5[1+1] := x4['s' if True else 'c'];

	for(x : int := 'c' -> ' ')
		 
		 x1 : real := '\\';
		 x2 : char := 'd';
		 x1 := x4[x2];

		 x4 : bool := "cia" = x2;
		 x4 := 1=1.0;
		 x4 := 1='c';
		 x4 := 10 = '\n';
		 x4 := "cia" <> 'c';
		 x4 := 'c' < 1;

	end for

	try 

		x1 : int := 1 if True else 2;
		x2 : bool := True if x1 = x1 else False;

		x3 : real := (x1 if x2 else 1) + 5 / 2  ;
		x3 := x1 %2 if x2 and x2 or ( x1 < ++x1) else x1%4;


		try catch end catch 

	catch

		try catch end catch

	end catch 


	for(x : int := 1 -> x)

		x6 : Array<Array<int>> := [[1,2,3,4],[1,2,3,4]];

		X7 : int := x6[1][2];

	end for



end main