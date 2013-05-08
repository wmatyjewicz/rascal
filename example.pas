function BoolToInt(b: boolean) : integer
begin
   if b then
      BoolToInt := 1
   else
      BoolToInt := 0
end

function Add(a : integer, b : integer) : integer
var sum :  integer;
begin
   sum := a + b;
   Add := sum
end

function Less(a : integer, b : integer) : boolean
begin
   Less := a < b
end

var x : integer;
    b : boolean;

begin
   BoolToInt(False);
   x := BoolToInt(True) + 2;
   b := Less(5, Add(x, 5))
end.
