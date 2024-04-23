; Declare Constant x
(declare-const x Int)
; Assert the negation of the verification condition:
; x > 0 && true ==> (0 <= x && 0 == 0 * x)
(assert (not (=> (and (> x 0) true) (and (<= 0 x) (= 0 (* 0 x))))))
; ask z3 to check it
(check-sat)
