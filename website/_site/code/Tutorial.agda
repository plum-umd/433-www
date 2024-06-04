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
   case split (C-c C-c), navigate goals (C-c C-f, C-c C-b), try
   to automatically fill holes (C-c C-a), fill them (C-c C-SPC)
   or refine them (C-c C-r).
   -}

_*_ : Nat → Nat → Nat
x * y = {!!}

{- Polymorphism:
   Note that this defines a function id that takes
   a type (something of type Set), and then can use
   that type in its own type -- more on that later!
-}

id : (A : Set) -> A -> A
id A x = {!!}

ex_id : Nat
ex_id = {!!}

{- To avoid providing type arguments, we can use curly
   braces to declare them implicit. -}

id' : {A : Set} → A → A
id' x = {!!}

ex_id' : Nat
ex_id' = {!!}

{- Polymorphic datatypes -}
data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

{- Operators can appear _everywhere_ -}

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B
infixr 4 _,_

fst : {A B : Set} → A × B -> A
fst p = {!!}

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
xs ++v ys = {!!}

{- Curry Howard Correspondence

   Key idea:
   Propositions correspond to  types
      Proofs    correspond to programs

   For each proposition, we can ask two questions:
   * How do we prove this proposition?
   * What can we deduce from it?

   Conjunction: P ∧ Q
   ------------------
   * Deduce:
     - Given a proof of P ∧ Q, we can deduce that both P and Q hold.
     - So given a proof r of P ∧ Q
       we can get a proof of P (let's call it _)
        and a proof of Q       (let's call it _).
   * To Prove:
     - We need a proof of P and a proof of Q.
     - A proof of P ∧ Q is a 





















   Therefore P ∧ Q corresponds to pairs P × Q.

   Disjunction: P ∨ Q
   ------------------
   * Deduce:
     - Given a proof of P ∧ Q, we can deduce that
     
   * To Prove
     - We need a proof of P _or_ a proof of Q.

   Therefore P ∨ Q correspons to ?
     -}

{- TODO: Data type for disjunction -}





















data Either (A B : Set) : Set where
  Left : A → Either A B
  Right : B → Either A B

{-

   Implication: P ⇒ Q
   ------------------
   * To Prove:
     - Given a proof of P, we need to construct a proof of Q.

   * Deduce:
     - Given a proof of P ⇒ Q, and a proof of P,
       we can combine the two proofs into a proof of Q.
     - ...

   Therefore P ⇒ Q corresponds to ? 

-}

{-
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

absurd : (A : Set) -> ⊥ -> A
absurd A f = {!!}

{- Now we can take _any_ formula in propositional logic, translate it
   to a type in Agda, and then prove the formula by writing down a
   program of that type! And if the program typechecks, it is a
   machine-checked proof of our original proposition! -}

{- Example: P implies P -}
proofEx1 : {P : Set} → P → P
proofEx1 p = {!!}

{- Example: Transitivity.
   (If P implies Q and Q implies R then P implies R)
-}

proofEx2 : {P Q R : Set} -> (P → Q) → (Q → R) → (P → R)
proofEx2 pq qr = {!!}

impossibleProof : {P Q : Set} → P → Q
impossibleProof p = {!!}

{- Predicate Logic -}

{- If propositions are types, then we can define new
   propositions by defining new data types. -}

data IsEven : Nat → Set where
  even-z  : IsEven z
  even-ss : {n : Nat} → IsEven n → IsEven (s (s n))
   
data IsOdd : Nat → Set where
  odd-sz  : IsOdd (s z)
  odd-ss : {n : Nat} → IsOdd n → IsOdd (s (s n))

2-is-even : IsEven two
2-is-even = {!!}

three : Nat
three = {!!}

1-is-not-even : IsEven (s z) → ⊥ 
1-is-not-even pf = {!!}

3-is-not-even : IsEven three → ⊥ 
3-is-not-even pf = {!!}

five : Nat
five = s (s (s (s (s z))))

5-is-not-even : IsEven five → ⊥
5-is-not-even pf = {!!} 

{- Quantification: -}
double : Nat → Nat
double z = z
double (s n) = s (s (double n))

double-is-even : (n : Nat) → IsEven (double n)
double-is-even n = {!!} 

ind-principle-nat : (P : Nat → Set) → P z → ({n : Nat} → P n → P (s n)) → ((n : Nat) → P n)
ind-principle-nat P pf-z pf-s n = {!!} 

open import Function.Base using (case_of_)

every-nat-is-even-or-odd : (n : Nat) → Either (IsEven n) (IsOdd n)
every-nat-is-even-or-odd n = {!!} 















{-
every-nat-is-even-or-odd z = Left even-z
every-nat-is-even-or-odd (s z) = Right odd-sz
every-nat-is-even-or-odd (s (s n)) =
  case (every-nat-is-even-or-odd n) of λ where
    (Left pf-even-n) → Left (even-ss pf-even-n)
    (Right pf-odd-n) → Right (odd-ss pf-odd-n)
-}
