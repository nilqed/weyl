

(in-package :weyl)

;; ge-variables

(defmacro ge-var (v) 
"Define a variable in the general domain (*general*). This is equivalent to
 (defvar v (coerce 'v *general*)). (CL::TYPE-OF v) ==> WEYLI::GE-VARIABLE."
  `(defvar ,v (coerce ',v *general*)))
  
(defun eval-str (s)
  (eval (read-from-string s)))


(defun ge-vars (vl)
"Define general variables from the list vl, that is looping of <ge-var> over
 vl. Examples: (ge-vars '(x_0 x_1 x_2 x_3)), (ge-vars '(x y z p q r))."
  (loop for i in vl
    do (eval-str (format nil "(ge-var ~a)" i))))
    
(defun list-ge-vars ()
"List all defined variables in domain *general*. One may check memmbership
 with (ge-variable? v)."
  (weyli::ge-variables *general*))
  
(defun set-latex-repr (var ltx)
"Add a LaTeX representation ltx (string '\\...') to a ge-variable var.
 Example: (set-latex-repr a \"\\alpha\")."
  (weyli::set-variable-property *general* var 'latex-repr ltx ))

;; infix -> prefix
(defun +-> (str)
"Convert a string containing a valid GE expression to prefix form.
 Examples: (+-> \"p+q\") ==> (+ p q), (+-> \"p*q^2\") ==> (* p (expt q 2))."
  (eval (read-from-string (format nil "~A" (infix:string->prefix str)))))


    