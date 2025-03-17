;(ql:quickload :weyl-test)

(declaim (sb-ext:muffle-conditions style-warning))

(in-package :weyl-test)


(format t "BEGIN WEYL TESTS ~%~%") 

(defvar tdb (lisp-unit:run-tests))
(terpri)
(describe tdb)
(terpri) 


(format t "TEST-NAMES: ....... ~A~%" (lisp-unit:test-names tdb))
(format t "FAILED-TESTS: ..... ~A~%" (lisp-unit:failed-tests tdb))
(format t "PRINT-FAILURES: ... ~A~%" (lisp-unit:print-failures tdb))
(format t "ERROR-TESTS: ...... ~A~%" (lisp-unit:error-tests tdb))
;
(lisp-unit:print-errors tdb)
(terpri)
(lisp-unit:summarize-results tdb)

(format t "~%END WEYL TESTS ~%~%") 
;(cl-user::quit)


