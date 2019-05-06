proc main()	is
	aa1 : int := 1;
	aa2 : bool:= True;
	aa3 : string := "prova";
	aa4 : char := 'p';
	const aa11 : int := 1;
	const aa22 : bool := True;
	const aa33 : string := "prova";
	const aa44 : char  := 'p';

	pozzo : bool := 1 < 1;
	pozzo := True = aa2;
	pozzo := True = False;
	pozzo := aa2 = False;
	pozzo := aa2 = aa2;
	pozzo := aa22 = aa22;
	pozzo := aa22 = True;
	pozzo := True = aa22;
	pozzo := aa44 = aa44;
	pozzo := aa44 = 'p';
	pozzo := 'p'  = aa44;
	pozzo := 'p' = 'p';
	pozzo := 2.5 = 2.5;

	pozzo := 1 <> 1;
	pozzo := True <> aa2;
	pozzo := True <> False;
	pozzo := aa2 <> False;
	pozzo := aa2 <> aa2;
	pozzo := aa22 <> aa22;
	pozzo := aa22 <> True;
	pozzo := True <> aa22;
	pozzo := aa44 <> aa44;
	pozzo := aa44 <> 'p';
	pozzo := 'p'  <> aa44;
	pozzo := 'p' <> 'p';
	pozzo := 2.5 <> 2.5;

	pozzo := aa1 < aa1 ;
	pozzo := aa1 > aa1 ;
	pozzo := aa1 <= aa1 ;
	pozzo := aa1 >= aa1 ;
	pozzo := aa1 <> aa1 ;
	pozzo := aa1 = aa1 ;

	pozzo := 1 < aa1 ;
	pozzo := 1 > aa1 ;
	pozzo := 1 <= aa1 ;
	pozzo := 1 >= aa1 ;
	pozzo := 1 <> aa1 ;
	pozzo := 1 = aa1 ;

	pozzo := aa1 < 1 ;
	pozzo := aa1 > 1 ;
	pozzo := aa1 <= 1 ;
	pozzo := aa1 >= 1 ;
	pozzo := aa1 <> 1 ;
	pozzo := aa1 = 1 ;

	pozzo := 1.0 < aa1 ;
	pozzo := 1.0 > aa1 ;
	pozzo := 1.0 <= aa1 ;
	pozzo := 1.0 >= aa1 ;
	pozzo := 1.0 <> aa1 ;
	pozzo := 1.0 = aa1 ;

	pozzo := aa1 < 1.0 ;
	pozzo := aa1 > 1.0 ;
	pozzo := aa1 <= 1.0 ;
	pozzo := aa1 >= 1.0 ;
	pozzo := aa1 <> 1.0 ;
	pozzo := aa1 = 1.0 ;

	pozzo := aa11 < aa11 ;
	pozzo := aa11 > aa11 ;
	pozzo := aa11 <= aa11 ;
	pozzo := aa11 >= aa11 ;
	pozzo := aa11 <> aa11 ;
	pozzo := aa11 = aa11 ;

	pozzo := 1 < aa11 ;
	pozzo := 1 > aa11 ;
	pozzo := 1 <= aa11 ;
	pozzo := 1 >= aa11 ;
	pozzo := 1 <> aa11 ;
	pozzo := 1 = aa11 ;

	pozzo := aa11 < 1 ;
	pozzo := aa11 > 1 ;
	pozzo := aa11 <= 1 ;
	pozzo := aa11 >= 1 ;
	pozzo := aa11 <> 1 ;
	pozzo := aa11 = 1 ;

	pozzo := 1.0 < aa11 ;
	pozzo := 1.0 > aa11 ;
	pozzo := 1.0 <= aa11 ;
	pozzo := 1.0 >= aa11 ;
	pozzo := 1.0 <> aa11 ;
	pozzo := 1.0 = aa11 ;

	pozzo := aa11 < 1.0 ;
	pozzo := aa11 > 1.0 ;
	pozzo := aa11 <= 1.0 ;
	pozzo := aa11 >= 1.0 ;
	pozzo := aa11 <> 1.0 ;
	pozzo := aa11 = 1.0 ;
==Undefined operator '< : bool x bool -> Bool'. (line: 109, column: 22) [TypeError].
	pozzo := aa2 < aa2 ;
==Undefined operator '> : bool x bool -> Bool'. (line: 111, column: 22) [TypeError].
	pozzo := aa2 > aa2 ;
==Undefined operator '<= : bool x bool -> Bool'. (line: 113, column: 22) [TypeError].
	pozzo := aa2 <= aa2 ;
==Undefined operator '>= : bool x bool -> Bool'. (line: 115, column: 22) [TypeError].
	pozzo := aa2 >= aa2 ;
	pozzo := aa2 <> aa2 ;
	pozzo := aa2 = aa2 ;

==Undefined operator '< : bool x bool -> Bool'. (line: 120, column: 23) [TypeError].
	pozzo := aa22 < aa22;
==Undefined operator '> : bool x bool -> Bool'. (line: 122, column: 23) [TypeError].
	pozzo := aa22 > aa22;
==Undefined operator '<= : bool x bool -> Bool'. (line: 124, column: 23) [TypeError].
	pozzo := aa22 <= aa22;
==Undefined operator '>= : bool x bool -> Bool'. (line: 126, column: 23) [TypeError].
	pozzo := aa22 >= aa22;
	pozzo := aa22 <> aa22;
	pozzo := aa22 = aa22;

==Undefined operator '< : bool x bool -> Bool'. (line: 131, column: 23) [TypeError].
	pozzo := aa22 < True;
==Undefined operator '> : bool x bool -> Bool'. (line: 133, column: 23) [TypeError].
	pozzo := aa22 > True;
==Undefined operator '<= : bool x bool -> Bool'. (line: 135, column: 23) [TypeError].
	pozzo := aa22 <= True;
==Undefined operator '>= : bool x bool -> Bool'. (line: 137, column: 23) [TypeError].
	pozzo := aa22 >= True;
	pozzo := aa22 <> True;
	pozzo := aa22 = True;

==Undefined operator '< : bool x bool -> Bool'. (line: 142, column: 23) [TypeError].
	pozzo := True < aa22;
==Undefined operator '> : bool x bool -> Bool'. (line: 144, column: 23) [TypeError].
	pozzo := True > aa22;
==Undefined operator '<= : bool x bool -> Bool'. (line: 146, column: 23) [TypeError].
	pozzo := True <= aa22;
==Undefined operator '>= : bool x bool -> Bool'. (line: 148, column: 23) [TypeError].
	pozzo := True >= aa22;
	pozzo := True <> aa22;
	pozzo := True = aa22;

==Undefined operator '< : string x string -> Bool'. (line: 153, column: 22) [TypeError].
	pozzo := aa3 < aa3;
==Undefined operator '> : string x string -> Bool'. (line: 155, column: 22) [TypeError].
	pozzo := aa3 > aa3;
==Undefined operator '<= : string x string -> Bool'. (line: 157, column: 22) [TypeError].
	pozzo := aa3 <= aa3;
==Undefined operator '>= : string x string -> Bool'. (line: 159, column: 22) [TypeError].
	pozzo := aa3 >= aa3;

	pozzo := aa3 <> aa3;

	pozzo := aa3 = aa3;

==Undefined operator '< : string x string -> Bool'. (line: 166, column: 23) [TypeError].
	pozzo := aa33 < aa3;
==Undefined operator '> : string x string -> Bool'. (line: 168, column: 23) [TypeError].
	pozzo := aa33 > aa3;
==Undefined operator '<= : string x string -> Bool'. (line: 170, column: 23) [TypeError].
	pozzo := aa33 <= aa3;
==Undefined operator '>= : string x string -> Bool'. (line: 172, column: 23) [TypeError].
	pozzo := aa33 >= aa3;

	pozzo := aa33 <> aa3;

	pozzo := aa33 = aa3;

==Undefined operator '< : string x string -> Bool'. (line: 179, column: 23) [TypeError].
	pozzo := aa33 < aa33;
==Undefined operator '> : string x string -> Bool'. (line: 181, column: 23) [TypeError].
	pozzo := aa33 > aa33;
==Undefined operator '<= : string x string -> Bool'. (line: 183, column: 23) [TypeError].
	pozzo := aa33 <= aa33;
==Undefined operator '>= : string x string -> Bool'. (line: 185, column: 23) [TypeError].
	pozzo := aa33 >= aa33;

	pozzo := aa33 <> aa33;

	pozzo := aa33 = aa33;

==Undefined operator '< : string x string -> Bool'. (line: 192, column: 23) [TypeError].
	pozzo := aa33 < "aa33";
==Undefined operator '> : string x string -> Bool'. (line: 194, column: 23) [TypeError].
	pozzo := aa33 > "aa33";
==Undefined operator '<= : string x string -> Bool'. (line: 196, column: 23) [TypeError].
	pozzo := aa33 <= "aa33";
==Undefined operator '>= : string x string -> Bool'. (line: 198, column: 23) [TypeError].
	pozzo := aa33 >= "aa33";

	pozzo := aa33 <> "aa33";

	pozzo := aa33 = "aa33";

==Undefined operator '< : string x string -> Bool'. (line: 205, column: 25) [TypeError].
	pozzo := "aa33" < "aa33";
==Undefined operator '> : string x string -> Bool'. (line: 207, column: 25) [TypeError].
	pozzo := "aa33" > "aa33";
==Undefined operator '<= : string x string -> Bool'. (line: 209, column: 25) [TypeError].
	pozzo := "aa33" <= "aa33";
==Undefined operator '>= : string x string -> Bool'. (line: 211, column: 25) [TypeError].
	pozzo := "aa33" >= "aa33";

	pozzo := "aa33" <> "aa33";

	pozzo := "aa33" = "aa33";

	pozzo := aa4 < aa4;
	pozzo := aa4 > aa4;
	pozzo := aa4 <= aa4;
	pozzo := aa4 >= aa4;
	pozzo := aa4 <> aa4;
	pozzo := aa4 = aa4;
	pozzo := aa4 < 'p';
	pozzo := aa4 > 'p';
	pozzo := aa4 <= 'p';
	pozzo := aa4 >= 'p';
	pozzo := aa4 <> 'p';
	pozzo := aa4 = 'p';
	pozzo := 'p' < aa4;
	pozzo := 'p' > aa4;
	pozzo := 'p' <= aa4;
	pozzo := 'p' >= aa4;
	pozzo := 'p' <> aa4;
	pozzo := 'p' = aa4;
	pozzo := 'p' < 'p';
	pozzo := 'p' > 'p';
	pozzo := 'p' <= 'p';
	pozzo := 'p' >= 'p';
	pozzo := 'p' <> 'p';
	pozzo := 'p' = 'p';

	pozzo := aa44 < aa44;
	pozzo := aa44 > aa44;
	pozzo := aa44 <= aa44;
	pozzo := aa44 >= aa44;
	pozzo := aa44 <> aa44;
	pozzo := aa44 = aa44;
	pozzo := aa44 < 'p';
	pozzo := aa44 > 'p';
	pozzo := aa44 <= 'p';
	pozzo := aa44 >= 'p';
	pozzo := aa44 <> 'p';
	pozzo := aa44 = 'p';
	pozzo := 'p' < aa44;
	pozzo := 'p' > aa44;
	pozzo := 'p' <= aa44;
	pozzo := 'p' >= aa44;
	pozzo := 'p' <> aa44;
	pozzo := 'p' = aa44;
	pozzo := 'p' < 'p';
	pozzo := 'p' > 'p';
	pozzo := 'p' <= 'p';
	pozzo := 'p' >= 'p';
	pozzo := 'p' <> 'p';
	pozzo := 'p' = 'p';

	arr : Array <int>:=[1,2,3];
==Undefined operator '< : Array <int>(3) x Array <int>(3) -> Bool'. (line: 269, column: 22) [TypeError].
	pozzo := arr < arr;
==Undefined operator '> : Array <int>(3) x Array <int>(3) -> Bool'. (line: 271, column: 22) [TypeError].
	pozzo := arr > arr;
==Undefined operator '<= : Array <int>(3) x Array <int>(3) -> Bool'. (line: 273, column: 22) [TypeError].
	pozzo := arr <= arr;
==Undefined operator '>= : Array <int>(3) x Array <int>(3) -> Bool'. (line: 275, column: 22) [TypeError].
	pozzo := arr >= arr;
==Undefined operator '<> : Array <int>(3) x Array <int>(3) -> Bool'. (line: 277, column: 22) [TypeError].
	pozzo := arr <> arr;
==Undefined operator '= : Array <int>(3) x Array <int>(3) -> Bool'. (line: 279, column: 22) [TypeError].
	pozzo := arr = arr;

==Undefined operator '< : Array <int>(3) x string -> Bool'. (line: 282, column: 22) [TypeError].
	pozzo := arr < "arr";
==Undefined operator '> : Array <int>(3) x int -> Bool'. (line: 284, column: 22) [TypeError].
	pozzo := arr > 1;
==Undefined operator '<= : Array <int>(3) x char -> Bool'. (line: 286, column: 22) [TypeError].
	pozzo := arr <= 'p';
==Undefined operator '>= : Array <int>(3) x Array <int>(3) -> Bool'. (line: 288, column: 22) [TypeError].
	pozzo := arr >= [1,2,3];
==Undefined operator '<> : Array <int>(3) x real -> Bool'. (line: 290, column: 22) [TypeError].
	pozzo := arr <> 1.7;

	point : °int := §aa1;

==Undefined operator '< : °int x °int -> Bool'. (line: 295, column: 24) [TypeError].
	pozzo := point < point;
==Undefined operator '> : °int x °int -> Bool'. (line: 297, column: 24) [TypeError].
	pozzo := point > point;
==Undefined operator '<= : °int x °int -> Bool'. (line: 299, column: 24) [TypeError].
	pozzo := point <= point;
==Undefined operator '>= : °int x °int -> Bool'. (line: 301, column: 24) [TypeError].
	pozzo := point >= point;
==Undefined operator '<> : °int x °int -> Bool'. (line: 303, column: 24) [TypeError].
	pozzo := point <> point;
==Undefined operator '= : °int x °int -> Bool'. (line: 305, column: 24) [TypeError].
	pozzo := point = point;

==Undefined operator '< : °int x string -> Bool'. (line: 308, column: 24) [TypeError].
	pozzo := point < "point";
==Undefined operator '> : °int x int -> Bool'. (line: 310, column: 24) [TypeError].
	pozzo := point > 1;
==Undefined operator '<= : °int x char -> Bool'. (line: 312, column: 24) [TypeError].
	pozzo := point <= 'p';
==Undefined operator '>= : °int x Array <int>(3) -> Bool'. (line: 314, column: 24) [TypeError].
	pozzo := point >= [1,2,3];
==Undefined operator '<> : °int x real -> Bool'. (line: 316, column: 24) [TypeError].
	pozzo := point <> 1.7;
==Undefined operator '= : °int x int -> Bool'. (line: 318, column: 24) [TypeError].
	pozzo := point = aa1;

end main