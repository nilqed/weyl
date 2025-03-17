# WEYL TESTS

    (ql:quickload :weyl-test)

## Results (Mon 17 Mar 23:21:12 CET 2025)

      kfp@omega:~$ lisp
      This is SBCL 2.2.9.debian, an implementation of ANSI Common Lisp.
      More information about SBCL is available at <http://www.sbcl.org/>.
      
      SBCL is free software, provided as is, with absolutely no warranty.
      It is mostly in the public domain; some portions are provided under
      BSD-style licenses.  See the CREDITS and COPYING files in the
      distribution for more information.
      * (ql:quickload :weyl-test)
      To load "weyl-test":
        Load 1 ASDF system:
          weyl-test
      ; Loading "weyl-test"
      .
      ;;; *************************************************************************
      ;;;   Infix notation for Common Lisp.
      ;;;   Version 1.3  28-JUN-96.
      ;;;   Written by Mark Kantrowitz, CMU School of Computer Science.
      ;;;   Copyright (c) 1993-95. All rights reserved.
      ;;;   May be freely redistributed, provided this notice is left intact.
      ;;;   This software is made available AS IS, without any warranty.
      ;;; *************************************************************************
      .BEGIN WEYL TESTS
      
      ...........
      Unit Test Summary
       | 61 assertions total
       | 55 passed
       | 6 failed
       | 0 execution errors
       | 0 missing tests
      
      
      #<TEST-RESULTS-DB Total(61) Passed(55) Failed(6) Errors(0)>..
        [standard-object]
      
      Slots with :INSTANCE allocation:
        DATABASE                       = #<HASH-TABLE :TEST EQ :COUNT 6 {1005B3C213}>
        PASS                           = 55
        FAIL                           = 6
        EXERR                          = 0
        FAILED-TESTS                   = (EXPAND GE-DERIV GE-BASICS)
        ERROR-TESTS                    = NIL
        MISSING-TESTS                  = NIL
      
      TEST-NAMES: ....... (GE-BASICS GE-DERIV EXPAND PERMUTE CHOOSE F-AND-G-SERIES)
      FAILED-TESTS: ..... (EXPAND GE-DERIV GE-BASICS)
       | Failed Form: (GE-EQUAL (EXPAND (EXPT (- P Q) 2))
                                (- (+ (* P P) (* Q Q)) (* 2 P Q)))
       | Expected T but saw NIL
       | (EXPAND (EXPT (- P Q) 2)) => (-1 q)^2 + p^2 - (2 q p)
       | (- (+ (* P P) (* Q Q)) (* 2 P Q)) => q^2 + p^2 - (2 q p)
       |
      EXPAND: 1 assertions passed, 1 failed.
      
       | Failed Form: (GE-EQUAL GE20 (- (/ (COS X) X) (/ (SIN X) (* X X))))
       | Expected T but saw NIL
       | GE20 => (cos(x)) x^-1 - (x^-2 (sin(x)))
       | (- (/ (COS X) X) (/ (SIN X) (* X X))) => (cos(x)) x^-1 - ((sin(x)) x^-2)
       |
       | Failed Form: (GE-EQUAL GE19 (/ (+ 1 (* X X)) (EXPT (- (* X X) 1) 2)))
       | Expected T but saw NIL
       | GE19 => (1 - x^2)^-1 + 2 (1 - x^2)^-2 x^2
       | (/ (+ 1 (* X X)) (EXPT (- (* X X) 1) 2)) => (1 + x^2) (-1 + x^2)^-2
       |
       | Failed Form: (GE-EQUAL GE18 0)
       | Expected T but saw NIL
       | GE18 => 2 (cosh(x)) (sinh(x)) - (2 (sinh(x)) (cosh(x)))
       | 0 => 0
       |
      GE-DERIV: 17 assertions passed, 3 failed.
      
       | Failed Form: (GE-EQUAL GE4 GE5)
       | Expected T but saw NIL
       | GE4 => (-1 q + p) (-1 q^2 + p^2)^-1 (q + p)
       | GE5 => 1
       |
       | Failed Form: (GE-EQUAL GE1 GE2)
       | Expected T but saw NIL
       | GE1 => (-1 q + p) (q + p)
       | GE2 => -1 q^2 + p^2
       |
      GE-BASICS: 3 assertions passed, 2 failed.
      
      PRINT-FAILURES: ... NIL
      ERROR-TESTS: ...... NIL
      
      Unit Test Summary
       | 61 assertions total
       | 55 passed
       | 6 failed
       | 0 execution errors
       | 0 missing tests
      
      
      END WEYL TESTS

      (:WEYL-TEST)
      *
      

## Comments
All failures are related to insufficient simplification  capabilities.
The `simplify` function has to be improved (WIP).


    
