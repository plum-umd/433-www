/* Exercise 1 (10 points)

Implement the following OCaml function as a dafny function:

let mystery_function x y z = 
  let a = x + y in
  let b = y + z in 
  let c = x + z in 
  (a+b+c) / 2
*/
function mystery_function(x:int,y:int,z:int):int {
    /* Replace with your definition */
    42
}

/* Exercise 2 (20 points)

You are given the following Dafny datatype, which is equivalent
to the following OCaml tree type:

type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

*/

datatype Tree<T> = 
    | Leaf 
    | Node (data:T, left: Tree<T>, right:Tree<T>)

/* Implement the `map` and `fold` functions over these trees.

For reference, the OCaml definition would be:

let rec mapTree f t = 
  match t with
  | Leaf -> Leaf
  | Node (x,l,r) -> Node (f x, mapTree f l, mapTree f r)

let rec foldTree f e t = 
  match t with
  | Leaf -> e 
  | Node (x,l,r) -> f x (foldTree f e l) (foldTree f e r)

Here is the stub for map:

*/
function mapTree<A,B> (f: A -> B, t: Tree<A>): Tree<B> {
    // Replace with your implementation
    Leaf 
}

/* Fill in your own template for fold, with the same argument order as the OCaml code. */

/* Exercise 3 (20 points)

You are given the following MapSet wrapper arround Dafny's maps, which
fixes the type of the "values" to be booleans. As a result,
one can think of a MapSet as a set, where an element is considered
to be in the MapSet iff it maps to true in the wrapped map.

Implement the following set API calls in terms of this 
wrapper, by invoking Dafny map functions as seen on the slides.

*/

datatype MapSet<T> = MapSet (s : map<T,bool>)

predicate member<T> (m:MapSet<T>, x:T) {
    // Replace with your definition
    true
}

function size<T> (m:MapSet<T>): int {
    // Replace with your definition
    42
}

function insert<T> (m:MapSet<T>, x:T): MapSet<T> {
    // Replace with your definition
    MapSet (map[])
}

function delete<T> (m:MapSet<T>, x:T): MapSet<T> {
    // Replace with your definition
    MapSet (map[])
}