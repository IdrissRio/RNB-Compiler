==Array and pointer test

const n : int := 3;

proc main () is
	a : int := 1;
	a1 : °int := §a;
	a2 : real := 2.1;
	a3 : °real := §a2;
	°a3 := a;
	a3 := a3; 
	a6 : real ;
	a7 : string := "ciao";
	a8 : °string := §a7;

	x : Array<real> := [1.1,2.0,a2,a2,°a3];  
	x1 : Array<real> :=  x;
	x2 : Array<Array<real>> := [x,x1,create Array<real>(5)];
	x1 := x2[1];
	x2[1] := x1; 
	x3 : Array<°Array<real>> := [§x1,§x,§x2[2],§x2[1]];
	x4 : Array<Array<°real>> := [[a3,§a2],[§a2,a3]];
	x5 : °Array<Array<real>> := §x2;
	x6 : Array<°Array<°real>> := [§x4[1]];

	a2 := (x2[1])[1];
	a2 := °(°(x6[1]))[1];

	x3[2] := §x;
	x2 := °x5;
	x4[2] := °x6[1];

	s1 : string := a7;
	s2 : Array<string> := [s1,"mondo",°a8];
	s3 : Array<Array<string>> := [s2];
	s4 : °Array<string> := §s2;

	s2[3]:= s1;
	s3[1][2] := "The almighty RNB Compilàà";
	s3[2][2] := s1;
	°s4 := s3[1];
	s4 := §s3[2];
end main