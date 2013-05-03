function BoolToInt(b: boolean) : integer
begin
   if b then
      BoolToInt := 1
   else
      BoolToInt := 0
end

var x : integer;

begin
   x := BoolToInt(True) + 2
end.
