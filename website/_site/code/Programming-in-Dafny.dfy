// Programming in Dafny, by example

// Write an XOR function - In class


// Convert the following ocaml expression into Dafny:
// let x = 42 in fst (x,x)
function test_tuple_let():int {
  42
}

// Write an identity function applied to 42.
// (fun x -> x) 42
function id_42(): int {
  42 
}

// Convert the following OCaml Program to Dafny
// data tree 'a = Leaf | Node of 'a * tree a * tree a 
// let rec size (t : tree 'a) : int = 
//   match t with 
//   | Leaf -> 0
//   | Node (x,l,r) -> 1 + size l + size r

// (==), 0, 00, !new

  datatype Status<T> =
    | Success(value: T)
    | Failure(error: string)
  {

    predicate IsFailure() { this.Failure? }

    function PropagateFailure(): Status<T> { this }

    function Extract(): T { this.value }

  }

// Write the following ocaml program in Dafny:
// let f x = Some x 
// let g y = 
//   match f y with 
//   | Some z -> Some (z * z)
//   | None -> None


// Write a method that initializes an array
method array_playground() returns (r : array<int>) {
}


method Main() {
  // print (xor(true, false));
//  print test_tuple_let(), "\n";
//  print id_42(), "\n";
//  print size(Node(42,Leaf,Leaf)), "\n";

}

/* Quiz:

Do the following OCaml - Dafny pairs represent the same program?

a) 

OCAML
let x = 42 in x + x

Dafny
var x := 42; x + x

(1) Yes  (2) No 

b)

OCAML
let x = (print (42); 42) in x + x

Dafny
var x := (print 42; 42); x + x

(1) Yes  (2) No 

QUIZ

Can you write the following in Dafny?
*/

method quiz3 () {
  var r : array<int>;
  r := new int[2];
  r[0] := 42;
}

/* 
(1) Yes  (2) No 

*/