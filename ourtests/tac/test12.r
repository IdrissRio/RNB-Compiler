proc main () is

x : int := 5;

arr : Array<Array<int>(3)>(2):= [[1,2,3],[4,5,6]];

fint(arr);

end main

proc fint(a : Array<Array<int>(3)>(2)) is
z : int := a[1][1];
end fint
