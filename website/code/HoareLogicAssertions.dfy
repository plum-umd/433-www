/** Hoare Triples and Dafny */

/** A _Hoare triple_ is a claim about the state before and
    after executing a statement.  The standard notation is

      {P} s {Q}

    meaning:

      - If statement s begins execution in a state satisfying assertion P,
      - and if s eventually terminates in some final state,
      - then that final state will satisfy the assertion Q.

    Assertion P is called the _precondition_ of the triple, and Q is
    the _postcondition_.

    For example,

    - {x == 0} x := x + 1 {x == 1} is a valid Hoare triple,
      stating that the statement x := x + 1 would transform a state in
      which x == 0 to a state in which x == 1.

    - forall m, {x == m} x := x + 1 {x == m + 1}, is a
      _proposition_ stating that the Hoare triple {x == m} x := x +
      m {x == m + 1} is valid for any choice of m.  Note that m
      in the two assertions and the command in the middle is a
      reference to the _Dafny_ variable m, which is bound outside the
      Hoare triple. */

/* ################################################################# */
/** * Proof Rules */

/** The goal of Hoare logic is to provide a _compositional_
    method for proving the validity of specific Hoare triples.  That
    is, we want the structure of a program's correctness proof to
    mirror the structure of the program itself.  To this end, in the
    sections below, we'll introduce a rule for reasoning about each of
    the different syntactic forms of statements in Dafny -- one for
    assignment, one for sequencing, one for conditionals, etc. -- plus
    a couple of "structural" rules for gluing things together.  We
    will then be able to prove programs correct using these proof
    rules. */

/* ================================================================= */
/** ** Empty statement blocks 

/** Since empty statement blocks don't change the state, they preserve any
    assertion [P]:

      --------------------  (hoare_skip)
         { P } {} { P }
*/

/* ================================================================= */
/** ** Sequencing 

/** If statement s1 takes any state where P holds to a state where
    Q holds, and if s2 takes any state where Q holds to one
    where R holds, then doing s1 followed by s2 will take any
    state where P holds to one where R holds:

        { P } s1 { Q }
        { Q } s2 { R }
       ----------------------  (hoare_seq)
       { P } s1;s2 { R }
*/

/* ================================================================= */
/** ** Assignment */

/** The rule for assignment is the most fundamental of the Hoare
    logic proof rules.  Here's how it works.

    Consider this incomplete Hoare triple:

       { ??? }}  x := y  { x == 1 }

    We want to assign y to x and finish in a state where x is 1.
    What could the precondition be?

    One possibility is y == 1, because if y is already 1 then
    assigning it to x causes x to be 1.  That leads to a valid
    Hoare triple:

       { y == 1 }  x := y  { x == 1 }

    It may seem as though coming up with that precondition must have
    taken some clever thought.  But there is a mechanical way we could
    have done it: if we take the postcondition x == 1 and in it
    replace x with y---that is, replace the left-hand side of the
    assignment statement with the right-hand side---we get the
    precondition, y = 1. 

    That same idea works in more complicated cases.  For
    example:

       { ??? }  x := x + y  { x == 1 }

    If we replace the x in x == 1 with x + y, we get x + y == 1.
    That again leads to a valid Hoare triple:

       { x + y == 1 }  x := x + y  { x == 1 }

    Why does this technique work?  The postcondition identifies some
    property P that we want to hold of the variable x being
    assigned.  In this case, P is "equals 1".  To complete the
    triple and make it valid, we need to identify a precondition that
    guarantees that property will hold of x.  Such a precondition
    must ensure that the same property holds of _whatever is being
    assigned to_ x.  So, in the example, we need "equals 1" to
    hold of x + y.  That's exactly what the technique guarantees. 

    In general, the postcondition could be some arbitrary assertion
    Q, and the right-hand side of the assignment could be some
    arbitrary arithmetic expression a:

       { ??? }  x := a  { Q }

    The precondition would then be Q, but with any occurrences of
    x in it replaced by a.

    Let's introduce a notation for this idea of replacing occurrences:
    Define Q[x := a] to mean "Q where a is substituted in
    place of x".

    This yields the Hoare logic rule for assignment:

      { Q[x := a] }  x := a  { Q }

    One way of reading this rule is: If you want statement x := a
    to terminate in a state that satisfies assertion Q, then it
    suffices to start in a state that also satisfies Q, except
    where a is substituted for every occurrence of x. *)

    To many people, this rule seems "backwards" at first, because
    it proceeds from the postcondition to the precondition.  Actually
    it makes good sense to go in this direction: the postcondition is
    often what is more important, because it characterizes what we
    can assume afer running the code.

    Nonetheless, it's also possible to formulate a "forward" assignment
    rule. 
*/

/** Here are some valid instances of the assignment rule:

      { (x <= 5) [x := x + 1] }   (that is, X + 1 <= 5)
        x := x + 1
      { x <= 5 }

      { (x == 3) [x := 3] }        (that is, 3 = 3)
        x := 3
      { x = 3 }

      { (0 <= x && x <= 5) [X := 3]  (that is, 0 <= 3 /\ 3 <= 5)
        x := 3
      { 0 <= x && x <= 5 }}
*/

/* ================================================================= */
/** ** Consequence 

    Sometimes the preconditions and postconditions we get from the
    Hoare rules won't quite be the ones we want in the particular
    situation at hand -- they may be logically equivalent but have a
    different syntactic form that fails to unify with the goal we are
    trying to prove, or they actually may be logically weaker (for
    preconditions) or stronger (for postconditions) than what we need. *)

    For instance,

      {(x == 3) [x := 3]} x := 3 {x == 3},

    follows directly from the assignment rule, but

      {true} x := 3 {x == 3}

    does not.  This triple is valid, but it is not an instance of
    [hoare_asgn] because true and (x == 3) [x := 3] are not
    syntactically equal assertions.

    However, they are logically _equivalent_, so if one triple is
    valid, then the other must certainly be as well.  We can capture
    this observation with the following rule:

                {P'} s {Q}
                  P <-> P'
         -------------------------   (hoare_consequence_pre_equiv)
                 {P} s {Q}
*)

(** Taking this line of thought a bit further, we can see that
    strengthening the precondition or weakening the postcondition of a
    valid triple always produces another valid triple. This
    observation is captured by two _Rules of Consequence_.

                {P'} c {Q}
                  P -> P'
         -----------------------------   (hoare_consequence_pre)
                {P} s {Q}

                {P} s {Q'}
                 Q' -> Q
         -----------------------------    (hoare_consequence_post)
                {P} s {Q}
*/

/* ================================================================= */
/** ** Conditionals 

    What sort of rule do we want for reasoning about conditional
    statements?

    Certainly, if the same assertion Q holds after executing
    either of the branches, then it holds after the whole conditional.
    So we might be tempted to write:

              {P} s1 {Q}
              {P} s2 {Q}
      ---------------------------------
       {P} if b { s1 } else { s2 } {Q}

    However, this is rather weak. For example, using this rule,
    we cannot show
   
      { true }
        if x == 0
          { y := 2 }
        else 
          { y := x + 1 }
      { x <= y }}
   
    since the rule tells us nothing about the state in which the
    assignments take place in the "then" and "else" branches.

    Fortunately, we can say something more precise.  In the
    "then" branch, we know that the boolean expression b evaluates to
    true, and in the "else" branch, we know it evaluates to false.
    Making this information available in the premises of the rule gives
    us more information to work with when reasoning about the behavior
    of s1 and s2 (i.e., the reasons why they establish the
    postcondition Q).

                {P &&  b} s1 {Q}
                {P && !b} s2 {Q}
      -------------------------------------  (hoare_if)
         {P} if b { s1 } else { s2 } {Q}
*/

/* ================================================================= */
/** ** While Loops 

/** The Hoare rule for while loops is based on the idea of an
    _invariant_: an assertion whose truth is guaranteed before and
    after executing a command.  An assertion [P] is an invariant of [c] if

      {{P}} c {{P}}

    holds.  Note that in the middle of executing [c], the invariant
    might temporarily become false, but by the end of [c], it must be
    restored. *)

(**  As a first attempt at a [while] rule, we could try:

             {{P}} c {{P}}
      ---------------------------
      {{P} while b do c end {{P}}

    That rule is valid: if [P] is an invariant of [c], as the premise
    requires, then no matter how many times the loop body executes,
    [P] is going to be true when the loop finally finishes.

    But the rule also omits two crucial pieces of information.  First,
    the loop terminates when [b] becomes false.  So we can strengthen
    the postcondition in the conclusion:

              {{P}} c {{P}}
      ---------------------------------
      {{P} while b do c end {{P /\ ~b}}

    Second, the loop body will be executed only if [b] is true.  So we
    can also strengthen the precondition in the premise:

            {{P /\ b}} c {{P}}
      --------------------------------- (hoare_while)
      {{P} while b do c end {{P /\ ~b}}
*)

(** That is the Hoare [while] rule.  Note how it combines
    aspects of [skip] and conditionals:

    - If the loop body executes zero times, the rule is like [skip] in
      that the precondition survives to become (part of) the
      postcondition.

    - Like a conditional, we can assume guard [b] holds on entry to
      the subcommand.
*)




/* ################################################################# */
/** * Decorated Programs */

/** The beauty of Hoare Logic is that it is _structure-guided_: the
    structure of proofs exactly follows the structure of programs.

    We can record the essential ideas of a Hoare-logic proof --
    omitting low-level calculational details -- by "decorating" a
    program with appropriate assertions on each of its commands.

    Such a _decorated program_ carries within itself an argument for
    its own correctness. 

    For example, consider the program: 
 */
method LoopToZero_1(m : int, p : int) returns (x: int, z: int) {
  x := m;
  z := p;
  while !(x == 0) {
    z := z - 1;
    x := x - 1;
  }
}

/* 
    Here is one possible specification for this program, in the
    form of a Hoare triple:

    { true }
    x := m;
    z := p;
    while !(x == 0) {
      z := z - 1;
      x := x - 1;
    }
    { z == p - m }

    and here is the equivalent specification in terms of 
    Dafny's _requires_ and _ensures_ syntax:
*/
method LoopToZero_2(m : int, p : int) returns (x: int, z: int)
  requires true
  ensures z == p - m
{
  x := m;
  z := p;
  while !(x == 0) {
    z := z - 1;
    x := x - 1;
  }
}

/** Here is a decorated version of this program, embodying a
    proof of this specification:

    { true } ->
    { m == m }
      x := m
    { x == m } ->
    { x == m && p == p };
      z := p;
    { x == m && z == p } ->
    { z - x == p - m }
      while !(x == 0) {
        { z - x == p - m && ! (x == 0) } ->
        { (z - 1) - (x - 1) == p - m }
        z := z - 1
        { z - (x - 1) == p - m };
        x := x - 1
        { z - x == p - m }
      end
    { z - x == p - m /\ !!(x == 0) } ->
    { z == p - m }}
*/

/** That is, a decorated program consists of the program's text
    interleaved with assertions (sometimes multiple assertions
    separated by implications). 

    A decorated program can be viewed as a compact representation of a
    proof in Hoare Logic: the assertions surrounding each command
    specify the Hoare triple to be proved for that part of the program
    using one of the Hoare Logic rules, and the structure of the
    program itself shows how to assemble all these individual steps
    into a proof for the whole program. *)

/** Dafny's goal is to verify such decorated programs "mostly
    automatically."  But, before we can verify anything, we need to be
    able to _find_ a proof for a given specification, and for this we
    need to discover the right assertions. This can be done in an
    almost mechanical way, with the exception of finding loop
    invariants. We'll first explain in detail how to construct 
    decorations for several short programs, all of which are 
    loop-free or have simple loop invariants. We'll return
    to finding more interesting loop invariants later. *)

(* ================================================================= *)
(** ** Example: Swapping *)

(** Consider the following program, which swaps the values of two
    variables using addition and subtraction (instead of by assigning
    to a temporary variable).

       x := x + y;
       y := x - y;
       x := x - y

    We can give a proof, in the form of decorations, that this program is
    correct -- i.e., it really swaps x and y -- as follows.

    (1)    { x == m && y == n } ->
    (2)    { (x + y) - ((x + y) - y) == n && (x + y) - y == m }
             x := x + y;
    (3)    { x - (x - y) == n && x - y == m }
             y := x - y;
    (4)    { x - y == n && y = m }
             x := x - y
    (5)    { x = n && y = m }

    The decorations can be constructed as follows:

      - We begin with the undecorated program (the unnumbered lines).

      - We add the specification -- i.e., the outer precondition (1)
        and postcondition (5). In the precondition, we use parameters
        m and n to remember the initial values of variables x
        and y so that we can refer to them in the postcondition (5).

      - We work backwards, mechanically, starting from (5) and
        proceeding until we get to (2). At each step, we obtain the
        precondition of the assignment from its postcondition by
        substituting the assigned variable with the right-hand-side of
        the assignment. For instance, we obtain (4) by substituting
        x with x - y in (5), and we obtain (3) by substituting y
        with x - y in (4).

    Finally, we verify that (1) logically implies (2) -- i.e., that
    the step from (1) to (2) is a valid use of the law of
    consequence -- by doing a bit of high-school algebra.
*/

(* ================================================================= *)
(** ** Example: Simple Conditionals *)

(** Here is a simple decorated program using conditionals:

      (1)   { true }
              if x <= y {
      (2)      { true && x <= y } ->
      (3)      { (y - x) + x == y || (y - x) + y == x }
                 z := y - x
      (4)      { z + x == y || z + y == x }
              } else {
      (5)       { true && ~(x <= y) } ->
      (6)       { (x - y) + x == y || (x - y) + y = x }
                z := x - y
      (7)       { z + x = y || z + y = x }
              }
      (8)   { z + x = y || z + y = x }}

These decorations can be constructed as follows:

  - We start with the outer precondition (1) and postcondition (8).

  - Following the format dictated by the [hoare_if] rule, we copy the
    postcondition (8) to (4) and (7). We conjoin the precondition (1)
    with the guard of the conditional to obtain (2). We conjoin (1)
    with the negated guard of the conditional to obtain (5).

  - In order to use the assignment rule and obtain (3), we substitute
    [Z] by [Y - X] in (4). To obtain (6) we substitute [Z] by [X - Y]
    in (7).

  - Finally, we verify that (2) implies (3) and (5) implies (6). Both
    of these implications crucially depend on the ordering of [X] and
    [Y] obtained from the guard. For instance, knowing that [X <= Y]
    ensures that subtracting [X] from [Y] and then adding back [X]
    produces [Y], as required by the first disjunct of (3). Similarly,
    knowing that [~ (X <= Y)] ensures that subtracting [Y] from [X]
    and then adding back [Y] produces [X], as needed by the second
    disjunct of (6). Note that [n - m + m = n] does _not_ hold for
    arbitrary natural numbers [n] and [m] (for example, [3 - 5 + 5 =
    5]). *)

(** **** Exercise: 2 stars, standard, optional (if_minus_plus_reloaded)

    N.b.: Although this exercise is marked optional, it is an
    excellent warm-up for the (non-optional) [if_minus_plus_correct]
    exercise below!

    Fill in valid decorations for the following program: *)
(*
  {{ True }}
    if X <= Y then
              {{                         }} ->>
              {{                         }}
      Z := Y - X
              {{                         }}
    else
              {{                         }} ->>
              {{                         }}
      Y := X + Z
              {{                         }}
    end
  {{ Y = X + Z }}
*)
(**
    Briefly justify each use of [->>].
*)

(** [] *)

(* ================================================================= *)
(** ** Example: Reduce to Zero *)

(** Here is a [while] loop that is so simple that [True] suffices
    as a loop invariant.

        (1)    {{ True }}
                 while X <> 0 do
        (2)                  {{ True /\ X <> 0 }} ->>
        (3)                  {{ True }}
                   X := X - 1
        (4)                  {{ True }}
                 end
        (5)    {{ True /\ ~(X <> 0) }} ->>
        (6)    {{ X = 0 }}

   The decorations can be constructed as follows:

     - Start with the outer precondition (1) and postcondition (6).

     - Following the format dictated by the [hoare_while] rule, we copy
       (1) to (4). We conjoin (1) with the guard to obtain (2). We also
       conjoin (1) with the negation of the guard to obtain (5).

     - Because the final postcondition (6) does not syntactically match (5),
       we add an implication between them.

     - Using the assignment rule with assertion (4), we trivially substitute
       and obtain assertion (3).

     - We add the implication between (2) and (3).

   Finally we check that the implications do hold; both are trivial. *)

(* ================================================================= *)
(** ** Example: Division *)

(** Let's do one more example of simple reasoning about a loop.

    The following Imp program calculates the integer quotient and
    remainder of parameters [m] and [n].

       X := m;
       Y := 0;
       while n <= X do
         X := X - n;
         Y := Y + 1
       end;

    If we replace [m] and [n] by concrete numbers and execute the program, it
    will terminate with the variable [X] set to the remainder when [m]
    is divided by [n] and [Y] set to the quotient. *)

(** In order to give a specification to this program we need to
    remember that dividing [m] by [n] produces a remainder [X] and a
    quotient [Y] such that [n * Y + X = m /\ X < n].

    It turns out that we get lucky with this program and don't have to
    think very hard about the loop invariant: the invariant is just
    the first conjunct, [n * Y + X = m], and we can use this to
    decorate the program.

      (1)  {{ True }} ->>
      (2)  {{ n * 0 + m = m }}
             X := m;
      (3)                     {{ n * 0 + X = m }}
             Y := 0;
      (4)                     {{ n * Y + X = m }}
             while n <= X do
      (5)                     {{ n * Y + X = m /\ n <= X }} ->>
      (6)                     {{ n * (Y + 1) + (X - n) = m }}
               X := X - n;
      (7)                     {{ n * (Y + 1) + X = m }}
               Y := Y + 1
      (8)                     {{ n * Y + X = m }}
             end
      (9)  {{ n * Y + X = m /\ ~ (n <= X) }} ->>
     (10)  {{ n * Y + X = m /\ X < n }}

    Assertions (4), (5), (8), and (9) are derived mechanically from
    the invariant and the loop's guard.  Assertions (8), (7), and (6)
    are derived using the assignment rule going backwards from (8)
    to (6).  Assertions (4), (3), and (2) are again backwards
    applications of the assignment rule.

    Now that we've decorated the program it only remains to check that
    the uses of the consequence rule are correct -- i.e., that (1)
    implies (2), that (5) implies (6), and that (9) implies (10). This
    is indeed the case:
      - (1) ->> (2):  trivial, by algebra.
      - (5) ->> (6):  because [n <= X], we are guaranteed that the
        subtraction in (6) does not get zero-truncated.  We can
        therefore rewrite (6) as [n * Y + n + X - n] and cancel the
        [n]s, which results in the left conjunct of (5).
      - (9) ->> (10):  if [~ (n <= X)] then [X < n].  That's straightforward
        from high-school algebra.
    So, we have a valid decorated program. *)
