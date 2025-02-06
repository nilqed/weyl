(ql:quickload :weyl-test)
(in-package :weyl-test)

(lisp-unit::run-tests '(f-and-g-series))
(lisp-unit::run-tests '(permute choose))
(defvar db (lisp-unit::run-tests '(ge-basics ge-deriv)))


(lisp-unit:test-names db)
(lisp-unit:print-failures db)
(lisp-unit:failed-tests db)
(lisp-unit:error-tests db)
(lisp-unit:print-errors db)
(lisp-unit:summarize-results db)


