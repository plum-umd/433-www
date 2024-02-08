method ComputeFib(n: nat) returns (b: nat)
{
   if n == 0 { return 0; }
   var i: int := 1;
   var a := 0;
       b := 1;
   while i < n
   {
      a, b := b, a + b;
      i := i + 1;
   }
}