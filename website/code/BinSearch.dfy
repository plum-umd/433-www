// Binary Search Method
method BinarySearch(a: array<int>, value: int) returns (index: int)
{
  var low, high := 0, a.Length;
  while low < high
  {
    var mid := (low + high) / 2;
    if a[mid] < value {
      low := mid + 1;
    } else if value < a[mid] {
      high := mid;
    } else {
      return mid;
    }
  }
  return -1;
}
