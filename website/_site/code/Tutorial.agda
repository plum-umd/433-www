{- Agda Tutorial -}

{-
   Agda is a dependently typed functional programming language that
   allows you to reason about the programs you write!

   To install Agda, you can just run:

     stack install Agda

   Warning, this might take a while! 
   Once installed, you can run to get things setup with emacs.

     agda-mode setup

   Then you can C-c C-l to load this (or any Agda) file interactively.
   Note - there is no syntax highlighting before loading the file.
   That's because the naming conventions in Agda are so liberal that
   we need type information to distinguish between terms, types, and
   other things! 
-}

{- Data Types: Natural Numbers -}

data Nat : Set where
  z : Nat
  s : Nat -> Nat

{- Defining datatypes is similar to the GADT syntax of Haskell -
   We're defining the Nat datatype, with two constructors, Z
   and S, the latter of which takes another Nat as an argument
   to construct a new one.

   Key differences:
   * The type annotation uses single colons.
   * Naming is liberal (e.g. constructors can be lower case)
   * The type of Nat is Set (similar to the kind * in Haskell)

   Defining a term is similar to Haskell. And a function on
   Nats is as well. 
-}

two : Nat
two = s (s z)

_+_ : Nat → Nat → Nat
z + y = y
(s x) + y = s (x + y)

{- The addition function is defined by pattern matching.
   Names between underscores define operators. The caveat
   is that you need to separate them by whitespace. Things
   like "1+1" are actually valid names!

   Also, note the arrows! Agda allows arbitrary unicode,
   so both -> and → are valid to construct function types. 
-}

{- Programming by case analysis: One of the cool features of Agda
   is support for type-guided interactive programming. You can
   add holes, and then ask the type checker for information (C-c C-e),
   case split (C-c C-c), navigate goals (C-c C-f, C-c C-b), or try
   to automatically fill holes (C-c C-a)
   -}

_*_ : Nat → Nat → Nat
x * y = {!!} 

{- Polymorphism:
   Note that this defines a function id that takes
   a type (something of type Set), and then can use
   that type in its own type -- more on that later!
-}

id : (A : Set) → A → A
id A x = x

ex_id : Nat
ex_id = id Nat z

{- To avoid providing type arguments, we can use curly
   braces to declare them implicit. -}

id' : {A : Set} → A → A
id' x = x

ex_id' : Nat
ex_id' = id' z

{- Polymorphic datatypes -}
data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

{- Operators can appear _everywhere_ -}

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B
infixr 4 _,_

fst : {A B : Set} → A × B -> A
fst (x , y) = x

{- Totality checking - Agda checks for:
   * Pattern completeness
   * Termination
-}

foo : Nat → Nat
foo x = {!!}

{- Dependent types

   Example: Vectors (length-indexed lists)
-}
data Vec (A : Set) : Nat → Set where
  {- Vector of length 0 -}
  []  : Vec A z
  {- Vector of length s n -}
  _::_ : {n : Nat} → A → Vec A n → Vec A (s n)
infixr 5 _::_

_++v_ : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (m + n)
[] ++v ys = ys
(x :: xs) ++v ys = x :: (xs ++v ys)

{- Curry Howard Correspondence

   Key idea:
   Propositions correspond to  types
      Proofs    correspond to programs

   For each proposition, we can ask two questions:
   * How do we prove this proposition?
   * What can we deduce from it?

   Conjunction: P ∧ Q
   ------------------
   * To Prove: We need a proof of P and a proof of Q.
     A proof of P ∧ Q is a pair (p,q) of two proofs,
     where p is a proof of P and q a proof of Q.
   * Deduce: If we know P ∧ Q, then we can deduce that
     both P and Q hold. So given a proof r of P ∧ Q
     we can get a proof of P (let's call it fst r)
     and a proof of Q (let's call it snd r).

   Therefore P ∧ Q corresponds to pairs P × Q.

   Implication: P ⇒ Q
   ------------------
   * To Prove: We can assume that a proof p of P holds
     and we need to construct a function λ x → q, where
     q is a proof of Q.
   * Deduce: If we have a proof f of P ⇒ Q, and a proof
     p of P, then we can combine the two proofs to
     create a proof of Q (f p).

   Therefore P ⇒ Q corresponds to function types P → Q.

   Disjunction: P ∨ Q
   ------------------
   * To Prove:
   * Deduce:

   Therefore P ∨ Q correspons to ?
  
   Truth: true
   -----------
   * To Prove: We can always prove true. Therefore we can
     assume there is some trivial proof tt of true.
   * Deduce: No new information.

   It corresponds to the following type (similar to unit
   in Haskell)
-}

data ⊤ : Set where
  tt : ⊤

{-
   Negation: false
   ---------------
   * To Prove:
   * Deduce:
-}

data ⊥ : Set where
  -- no constructors


{- Now we can take _any_ formula in propositional logic, translate it
   to a type in Agda, and then prove the formula by writing down a
   program of that type! And if the program typechecks, it is a
   machine-checked proof of our original proposition! -}

{- Example: P implies P -}
proofEx1 : {P : Set} → P → P
proofEx1 p = {!!}

{- Example: Transitivity.
   (If P implies Q and Q implies R then P implies R)

TODO: in class. -}

{- Predicate Logic -}

{- If propositions are types, then we can define new
   propositions by defining new data types. -}

data IsEven : Nat → Set where
  even-z  : IsEven z
  even-ss : {n : Nat} → IsEven n → IsEven (s (s n))
   
2-is-even : IsEven two
2-is-even = {!!}
   
{- Quantification: -}
double : Nat → Nat
double z = z
double (s n) = s (s (double n))

double-is-even : (n : Nat) → IsEven (double n)
double-is-even n = ?
