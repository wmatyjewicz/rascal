function BoolToInt(b: boolean) : integer
begin
   if b then
      BoolToInt := 1
   else
      BoolToInt := 0
end

function Add(a : integer, b : integer) : integer
var sum : integer;
begin
   sum := a + b;
   Add := sum
end

function Less(a : integer, b : integer) : boolean
begin
   Less := a < b
end

function Sum() : integer
var x : integer;
begin
   ReadLn(x);
   while x <> 0 do begin
      Sum := Sum + x;
      ReadLn(x);
   end
end

var x : integer;
    y : integer;

begin
   ReadLn(x);
   ReadLn(y);
   WriteLn(BoolToInt(Less(x, y)));
   WriteLn(Add(x, y));

   WriteLn(Sum())
end.
