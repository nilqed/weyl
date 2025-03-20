(in-package :weyl)

(defun wtype (obj) (cl::type-of obj))

#|
-------------
WEYLI CLASSES
-------------

RATIONAL-INTEGER
RATIONAL-NUMBER
WEYLI::GE-ATOM
WEYLI::GE-NARY
WEYLI::GE-VARIABLE
WEYLI::GE-TIMES
WEYLI::GE-PLUS
WEYLI::GE-EXPT
WEYLI::GE-FUNCTION
WEYLI::GE-FUNCTION-DERIV
WEYLI::GE-APPLICATION
WEYLI::GE-EQUATION
WEYLI::GE-EQN=
WEYLI::GE-EQN>
WEYLI::GE-EQN>=
WEYLI::GE-FOURIER


WEYLI::GENERAL-EXPRESSIONS
WEYLI::GENERAL-EXPRESSION
WEYLI::ABSTRACT-FUNCTION
WEYLI::APPLICABLE-FUNCTION
WEYLI::UNIVERSAL-QUANTIFIED-SET


|#

(defgeneric latex (object &key &allow-other-keys)
  (:documentation "Return a LaTeX representation of object."))

;; reader? -- weyli::integer-value
(defmethod latex ((x weyli::rational-integer) &key (pre "") (post ""))
  (format nil "~A{~A}~A" pre (slot-value x 'weyli::value) post)) 

(defmethod latex ((x weyli::rational-number) &key (pre "") (post ""))
  (format nil "~A{\\frac{~A}{~A}}~A" 
     pre (numerator x) (denominator x) post)) 

(defmethod latex ((x weyli::ge-variable) &key (pre "") (post ""))
  (let* ((lr (get-variable-property *general* x 'latex-repr)))
    (if lr (format nil "~A{~A}~A" pre lr post)
      (format nil "~A{~A}~A" pre (string-of x) post))))

(defmethod latex ((x weyli::ge-expt) &key (pre "") (post ""))
  (let ((bas (base-of x)) (exp (exponent-of x)))
    (format nil "{~A{~A}^{~A}~A}" 
     pre 
      (case (wtype bas)
        (weyli::rational-integer (latex bas))
        (weyli::ge-variable      (latex bas))
        (weyli::ge-expt          (latex bas))
        (weyli::ge-application   (latex bas))
        (otherwise (latex bas :pre "(" :post ")")))
      (case (wtype exp)
        (weyli::rational-integer (latex exp))
        (weyli::ge-variable      (latex exp))
        (weyli::ge-expt          (latex exp))
        (weyli::ge-application   (latex exp))
        (otherwise (latex exp :pre "(" :post ")")))        
     post)))

(defmethod latex ((x weyli::ge-plus) &key (pre "") (post ""))
  (format nil "~A(~{{~A}~^ + ~})~A" 
    pre (mapcar #'latex (terms-of x)) post))

(defmethod latex ((x weyli::ge-times) &key (pre "") (post ""))
  (format nil "~A~{{~A}~^ \\, ~}~A" 
    pre (mapcar #'latex (terms-of x)) post))

(defmethod latex ((x weyli::ge-application) &key (pre "") (post ""))
  (format nil "~A\\operatorname{~A}( ~{{~A}~^ , ~})~A" 
    pre 
     (funct-of x)
     (mapcar #'latex (args-of x)) 
    post))  

(defmethod latex ((x weyli::ge-eqn=) &key (pre "") (post ""))
  (format nil "~A{~A=~A}~A" pre (latex (lhs-of x)) 
                     (latex (rhs-of x)) post))

(defmethod latex ((x weyli::ge-eqn>) &key (pre "") (post ""))
  (format nil "~A{~A>~A}~A" pre (latex (lhs-of x)) 
                     (latex (rhs-of x)) post))

(defmethod latex ((x weyli::ge-eqn>=) &key (pre "") (post ""))
  (format nil "~A{~A\\geq ~A}~A" pre (latex (lhs-of x)) 
                     (latex (rhs-of x)) post)) 
                     




  
 
 
