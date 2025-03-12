;;; provisional -- testing LaTeX in MathJax, Jupyter and Sixel.
;;; (weyli::set-variable-property *general* var  'latex-repr "\\??" )
;;; Later we will use a generic latex method for all weyl types:
;;; (defmethod latex ((obj weyl-type) &key (pre "") (post "") (context nil))

#|
(in-package :weyl)

(defun wtype (obj) (cl::type-of obj))


(defun ltx-ratint (x)
  (format nil "{~A}" (slot-value x 'weyli::value)))  

(defun ltx-var (x) ;;TODO: subscripts
  (let ((lr (get-variable-property *general* x 'latex-repr )))
    (if lr (format nil "{~A}" lr)
      (format nil "{~A}" (slot-value x 'weyli::string)))))
  
(defun ltx-expt (x)
  (format nil "{{~A}^{~A}}" 
    (ltx (slot-value x 'weyli::base))
    (ltx (slot-value x 'weyli::exp))))
    
(defun ltx-plus (x)
  (format nil "~{{~a}~^ + ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))

(defun ltx-times (x)
  (format nil "~{{~a}~^ \\, ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))
  
(defun ltx-app (x)
  (format nil "\\operatorname{~a}( ~{{~a}~^ , ~})" 
    (slot-value x 'weyli::funct)
    (mapcar 'ltx (slot-value x 'weyli::args))))

(defun ltx (x)
  (case (wtype x) 
    (rational-integer       (ltx-ratint x))
    (weyli::ge-variable     (ltx-var x))
    (weyli::ge-expt         (ltx-expt x))
    (weyli::ge-plus         (ltx-plus x))
    (weyli::ge-times        (ltx-times x))
    (weyli::ge-application  (ltx-app x))
    (otherwise "NOT IMPLEMENTED YET"))) 



(defun latex (x)
"Tries to represent object x as latex code. Example: (latex (expt p q )) ==>
 $${{{\\pi}}^{{q}}}$$"
  (format nil "$$~A$$" (ltx x)))
  
|#  
;;;; latex->sixel

(in-package :cl-user)

(defmacro temp-basename (prefix)
  `(let ((a (write-to-string (get-universal-time)))
         (b (write-to-string (random 1000))))
         (format nil "~A-~A-~A" ,prefix a b)))


(defparameter +temp+ "/tmp")


(defparameter tex-template "\\documentclass[~A]{article}
\\usepackage{amsmath,amssymb}
\\usepackage{breqn}
\\pagestyle{empty}
\\begin{document}
 ~A
\\end{document}")

(defparameter latex-cmd 
"latex -jobname=~A --output-directory=~A -interaction=nonstopmode ~A")

(defparameter dvipng-cmd 
"dvipng -T ~A -D ~A -O ~A -fg ~A -bg ~A -q -o ~A ~A")

(defparameter image-to-sixel-cmd
"img2sixel ~A")

(defun write-tex-file (jobname tex &key (pt "11pt"))
  (let ((file-name (concatenate 'string +temp+ "/" jobname ".tex")))
    (with-open-file (texf file-name
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)
        (format texf tex-template pt tex))))
        
(defun run-latex (jobname)
  (let* ((file-name (concatenate 'string +temp+ "/" jobname ".tex"))
        (cmd (format nil latex-cmd jobname +temp+ file-name)))
        (uiop:run-program cmd :output nil :ignore-error-status t)))
  

(defun run-dvipng (jobname fg bg res size off)
  (let* ((file-name (concatenate 'string +temp+ "/" jobname ".dvi"))
         (img (concatenate 'string +temp+ "/" jobname ".png")) 
         (cmd (format nil dvipng-cmd size res off fg bg img file-name)))
         (uiop:run-program cmd :output nil :ignore-error-status t)))


(defun run-img-to-sixel (jobname)
  (let* ((img (concatenate 'string +temp+ "/" jobname ".png"))
        (cmd (format nil image-to-sixel-cmd img)))
        (uiop:run-program cmd :output t :ignore-error-status t)))
        
(defun tidy-up (jobname) jobname) 
  ;(let ((cmd (format nil "rm ~A/~A.*" +temp+ jobname)))
    ;(uiop:run-program cmd :output nil :ignore-error-status t)))
       

(defun latex-to-sixel (tex  &key (pt "11pt") (fg "Green") (bg "Black") 
             (res "150") (size "bbox") (off "-1.0cm,-2.0cm"))
"Render latex code on a sixel-graphics capable terminal (xterm, mlterm, ...).
 Besides the LaTex string, optional keys are: :pt point-size, :fg foreground-
 color, :bg background-color, :res resolution, :size size and :off offset."
  (let* ((jobname (temp-basename "ltx2sixel")))
         (progn (write-tex-file jobname tex :pt pt)
                (run-latex jobname)
                (run-dvipng jobname fg bg res size off)
                (run-img-to-sixel jobname)
                (tidy-up jobname))))


(defun image-to-sixel (file)
"Render a .png, .jpg etc. picture on a sixel-graphics capable terminal."
  (uiop:run-program 
    (format nil "img2sixel ~A" file) :output t :ignore-error-status t))
    

(in-package :weyl)

(defun display6 (obj &key (pt "11pt") (fg "Green") (bg "Black") 
             (res "150") (size "bbox") (off "-1.0cm,-2.0cm"))
"Display a object as rendered LaTeX code in a terminal that supports sixel
 graphics (e.g. xterm, mlterm and some others)."
  (progn 
    (cl-user::latex-to-sixel 
      (latex obj :pre "$$" :post "$$") 
        :fg "Blue" :bg "'rgb 1.0 1.0 1.0'" ) T)) 
  



