==Dead code after 'return' statement. (line: 4, column: 1) [ControlFlowAnalysis].
==The return variable 'x' may be never initialized. (line: 3, column: 17) [ControlFlowAnalysis].
func main() -> (x: string) is
return;
	case 1 of
	1 -> x := "true";
	2 -> x := "true";
	else
		 x :="true";
	end case
end main

==The return variable 'x' may be never initialized. (line: 14, column: 18) [ControlFlowAnalysis].
func main1() -> (x : int) is
	
end main1

func main2() -> (x : int) is
	x := (10);
	return;
end main2
==Infinite loop. (line: 24, column: 9) [ControlFlowAnalysis].
func main3() -> (x : string) is
	loop
		x := "prova";
	end loop
end main3

==The return variable 'x' may be never initialized. (line: 30, column: 18) [ControlFlowAnalysis].
func main4() -> (x : string) is
	loop
		exit;
		x := "prova";
	end loop
end main4

func main5() -> (x : string) is
	loop
		x := "prova";
		exit;
		x := "prova";
	end loop
end main5

==The return variable 'x' may be never initialized. (line: 46, column: 18) [ControlFlowAnalysis].
func main6() -> (x : string) is
	if (True) then
		x := "prova";
	end if
end main6

func main7() -> (x : string) is
	if (True) then
		x := "prova";
	else
		x := "prova";
	end if
end main7

func main8() -> (x : string) is
	if (True) then
		if (True) then
			x := "prova";
		else
				if (True) then
				x := "prova";
			else
				x := "prova";
			end if
		end if
	else
		if (True) then
			x := "prova";
		else
			x := "prova";
		end if
	end if
end main8

==The return variable 'x' may be never initialized. (line: 81, column: 18) [ControlFlowAnalysis].
func main9() -> (x : string) is
	if (True) then
		if (True) then
			x := "prova";
		else
			if (True) then
				x := "prova";
			end if
		end if
	else
		if (True) then
			x := "prova";
		else
			x := "prova";
		end if
	end if
end main9



func main10() -> (x : string) is
	if (True) then
		if (True) then
			x := "prova";
		else
			case True of
				True -> x := "prova";
				else x := "prova";
			end case
		end if
	else
		if (True) then
			x := "prova";
		else
			x := "prova";
		end if
	end if
end main10

==The return variable 'x' may be never initialized. (line: 121, column: 19) [ControlFlowAnalysis].
func main11() -> (x : string) is
	if (True) then
		if (True) then
			x := "prova";
		else
			case True of
				True -> x := "prova";
			end case
		end if
	else
		if (True) then
			x := "prova";
		else
			x := "prova";
		end if
	end if
end main11

func main12() -> (x:string) is
	if (True) then
		x := "prova";
	elseif (True) then
		x := "prova";
	else 
		x := "prova";
	end if
end main12

==The return variable 'x' may be never initialized. (line: 150, column: 19) [ControlFlowAnalysis].
func main13() -> (x:string) is
	if (True) then
		x := "prova";
	elseif (True) then
		x := "prova";
	else 
		
	end if
end main13
==The return variable 'x' may be never initialized. (line: 160, column: 19) [ControlFlowAnalysis].
func main14() -> (x:string) is
	if (True) then
		x := "prova";
	elseif (True) then
		
	else 
		x := "prova";
	end if
end main14

