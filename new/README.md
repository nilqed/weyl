# New files

* ge-support
* ge-latex
* weyl-inifx


## ge-support

in package :weyl

ge-var

    (documentation 'ge-var 'function)
    "Define a variable in the general domain (*general*). This is equivalent to
    (defvar v (coerce 'v *general*)). (CL::TYPE-OF v) ==> WEYLI::GE-VARIABLE."

ge-vars

    (documentation 'ge-vars 'function)
    "Define general variables from the list vl, that is looping of <ge-var> over
    vl. Examples: (ge-vars '(x_0 x_1 x_2 x_3)), (ge-vars '(x y z p q r))."


list-ge-vars

    (documentation 'list-ge-vars 'function)
    "List all defined variables in domain *general*. One may check memmbership
    with (ge-variable? v)."

set-latex-repr

    (documentation 'set-latex-repr 'function)
    "Add a LaTeX representation ltx (string '\\...') to a ge-variable var.
    Example: (set-latex-repr a \"\\alpha\")." 

`+->` (inifx->prefix)
 
    (documentation '+-> 'function)
    "Convert a string containing a valid GE expression to prefix form.
    Examples: (+-> \"p+q\") ==> (+ p q), (+-> \"p*q^2\") ==> (* p (expt q 2))."


## ge-latex

latex

    (documentation 'latex 'function)
    "Tries to represent object x as latex code. Example: (latex (expt p q )) ==>
    $${{{\\pi}}^{{q}}}$$"

cl-user::latex-to-sixel

    (documentation 'cl-user::latex-to-sixel 'function)
    "Render latex code on a sixel-graphics capable terminal (xterm, mlterm, ...).
    Besides the LaTex string, optional keys are: :pt point-size, :fg foreground-
    color, :bg background-color, :res resolution, :size size and :off offset."

display6

    (documentation 'display6 'function)
    "Display a object as rendered LaTeX code in a terminal that supports sixel
    graphics (e.g. xterm, mlterm and some others)."


![display6](https://github.com/nilqed/cl-weyl/tree/main/docs/display6.png)

:todo: handle (-1)

In Jupyter notebooks (using e.g. [juCL](https://github.com/nilqed/juCL)) we can use `(latex ...)`, even with
TeXmacs;  [texmacs-plugin](https://github.com/nilqed/sbcl-texmacs).

![jucl-ge-latex](https://github.com/nilqed/cl-weyl/tree/main/docs/jucl-ge-latex.png)    

## weyl-infix

    ;;; Wed Jan 18 13:13:59 1995 by Mark Kantrowitz <mkant@FLATHEAD.OZ.CS.CMU.EDU>
    ;;; infix.cl -- 40545 bytes

    ;;; **************************************************************************
    ;;; Infix ********************************************************************
    ;;; **************************************************************************
    ;;;
    ;;; This is an implementation of an infix reader macro. It should run in any
    ;;; valid Common Lisp and has been tested in Allegro CL 4.1, Lucid CL 4.0.1,
    ;;; MCL 2.0 and CMU CL. It allows the user to type arithmetic expressions in
    ;;; the traditional way (e.g., 1+2) when writing Lisp programs instead of
    ;;; using the normal Lisp syntax (e.g., (+ 1 2)).  It is not intended to be a
    ;;; full replacement for the normal Lisp syntax. If you want a more complete
    ;;; alternate syntax for Lisp, get a copy Apple's MLisp or Pratt's CGOL.
    ;;;

We only swapped  `^^` (former `expt`)  and `^` (former `logor`) and use eval/read-from-string to
avoid package tags.





