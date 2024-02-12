
// Linear Search in an Array
// It returns the index of the key, if it finds it
// It returns -1, if the key is not in the array
method Find(a: array<int>, key: int) returns (index: int)
  requires a.Length > 0 
  ensures 0 <= index < a.Length ==> a[index] == key
  ensures index < 0 ==> forall k :: 0 <= k < a.Length ==> a[k] != key
{
  index := 0;
  while index < a.Length
    invariant index <= a.Length
    invariant forall k :: 0 <= k < index ==> a[k] != key
  {
    if a[index] == key 
    { 
      return; 
      // {{ a[index] == key }}
    }
    index := index + 1;
  }
  index := -1;
  // {{ forall k :: 0 <= k < a.Length ==> a[k] != key }}
}

