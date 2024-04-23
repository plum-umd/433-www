; Declare constants and functions
(declare-const x Int)
; Assert the negation of your verification condition
(assert (not (=> true (= 42 42))))
; Check for satisfiability
(check-sat)
