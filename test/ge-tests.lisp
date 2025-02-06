;;;;  Weyl Unit Testing

(in-package :weyl-test)

(lisp-unit:define-test ge-basics
  (let* ((p (coerce 'p *general*))
         (q (coerce 'q *general*))
         (x (coerce 'x *general*))
         (y (coerce 'y *general*))
         (z (coerce 'z *general*))
         (ge1 (* (+ p q) (- p q)))
         (ge2 (- (* p p) (* q q)))
         (ge3 (- (expt p 2) (expt q 2)))
         (ge4 (/ ge1 ge2))
         (ge5 (/ ge2 ge3))
         )
         (lisp-unit:assert-true (ge-equal ge1 ge2))
         (lisp-unit:assert-true (ge-equal ge2 ge3))
         (lisp-unit:assert-true (ge-equal (expand ge1) ge2))
         (lisp-unit:assert-true (ge-equal ge5 1))
         (lisp-unit:assert-true (ge-equal ge4 ge5))))
         
(lisp-unit:define-test ge-deriv
  (let* ((p (coerce 'p *general*))
         (q (coerce 'q *general*))
         (x (coerce 'x *general*))
         (y (coerce 'y *general*))
         (z (coerce 'z *general*))
         (ge1 (deriv (* p q) p))
         (ge2 (deriv (expt p 13) p))
         (ge3 (deriv (expt p q) p))
         ;(ge4 (deriv (expt p q) q)) not impl
         (ge5 (deriv (sin p) p))
         )
         (lisp-unit:assert-true (ge-equal ge1 q))
         (lisp-unit:assert-true (ge-equal ge2 (* (expt p 12) 13)))
         (lisp-unit:assert-true (ge-equal ge3 (* (expt p (- q 1)) q)))
         ;(lisp-unit:assert-true (ge-equal ge5 ???))
         (lisp-unit:assert-true (ge-equal ge5 (cos p)))))
     