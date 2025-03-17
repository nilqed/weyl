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
  (let* 
    ((p (coerce 'p *general*))
     (q (coerce 'q *general*))
     (x (coerce 'x *general*))
     (y (coerce 'y *general*))
     (z (coerce 'z *general*))
     (ge1  (deriv (* p q) p))
     (ge2  (deriv (expt p 13) p))
     (ge3  (deriv (expt p q) p))
     (ge4  (deriv (expt p q) q))
     (ge5  (deriv (sin p) p))
     (ge6  (* (log p) (expt p q)))
     (ge7  (deriv (expt p q) q q q))
     (ge8  (deriv (expt p q) q q q q))   
     (ge9  (deriv (expt p (* q q)) q))
     (ge10 (deriv (expt p (* q q)) p))
     (ge11 (deriv (expt (cos q)  (sin q)) q))
     (ge12 (deriv (expt p q) q q q q q q q q q q))
     (ge13 (deriv (log x) x))
     (ge14 (deriv (* x (log x)) x))
     (ge15 (deriv (sinh x) x))
     (ge16 (deriv (cosh x) x))
     (ge17 (deriv (tanh x) x))
     (ge18 (deriv (- (expt (cosh x) 2) (expt (sinh x) 2)) x))
     (ge19 (deriv (/ x (- 1 (* x x))) x))
     (ge20 (deriv (/ (sin x) x) x)))
     
     (lisp-unit:assert-true (ge-equal ge1 q))
     (lisp-unit:assert-true (ge-equal ge2  (* (expt p 12) 13)))
     (lisp-unit:assert-true (ge-equal ge3  (* (expt p (- q 1)) q)))
     (lisp-unit:assert-true (ge-equal ge4  ge6))
     (lisp-unit:assert-true (ge-equal ge5  (cos p)))
     (lisp-unit:assert-true (ge-equal ge6  ge4))
     (lisp-unit:assert-true (ge-equal ge7  (* (expt (log p) 3) (expt p q))))
     (lisp-unit:assert-true (ge-equal ge8  (* (expt (log p) 4) (expt p q))))
     (lisp-unit:assert-true (ge-equal ge9  (* 2 (log p) q (expt p (* q q)))))
     (lisp-unit:assert-true (ge-equal ge10 (* (* q q) (expt p (- (* q q) 1)))))
     (lisp-unit:assert-true (ge-equal ge11 (- (* (log (cos q)) (expt (cos q) (+ (sin q) 1)))
                          (* (expt (sin q) 2) (expt (cos q) (- (sin q) 1))))))
     (lisp-unit:assert-true (ge-equal ge12 (* (expt (log p) 10) (expt p q))))
     (lisp-unit:assert-true (ge-equal ge13 (/ 1 x)))
     (lisp-unit:assert-true (ge-equal ge14 (+ 1 (log x))))
     (lisp-unit:assert-true (ge-equal ge15 (cosh x)))
     (lisp-unit:assert-true (ge-equal ge16 (sinh x)))
     (lisp-unit:assert-true (ge-equal ge17 (/ 1 (expt (cosh x) 2))))
     (lisp-unit:assert-true (ge-equal ge18 0))
     (lisp-unit:assert-true (ge-equal ge19 (/ (+ 1 (* x x)) (expt (- (* x x) 1) 2))))         
     (lisp-unit:assert-true (ge-equal ge20 (- (/ (cos x) x) (/ (sin x) (* x x)))))))
     
(lisp-unit:define-test expand
  (let* 
    ((p (coerce 'p *general*))
     (q (coerce 'q *general*)))
     (lisp-unit:assert-true (ge-equal (expand (* (- p q) (+ p q))) (- (* p p) (* q q))))
     (lisp-unit:assert-true (ge-equal (expand (expt (- p q) 2)) (- (+ (* p p) (* q q)) (* 2 p q))))
))     
     
     