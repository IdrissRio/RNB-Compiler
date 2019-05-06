func prova(x :Array<int>(2)) -> (z : Array<int>(2)) is
	y : Array<int> := [1,2];
	z := y;
end prova


proc main() is
	x : Array<int> := [4,5];
	x := prova(x);

end main