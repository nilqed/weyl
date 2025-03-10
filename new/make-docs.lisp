;;;; make-docs.lisp 
;;;;   (create-refman) builds the WEYL reference manual (HTML)
;;;;    from scratch based on:
;;;;    -- weyl-folder [PARAMETER]   .... the path of the weyl folder
;;;;    -- src-files   [PARAMETER]   .... list of source files 
;;;;    -- html-[index, nav, start]  .... html templates
;;;; The output is in <weyl-folder>/docs.
;;;; Note that files are superseded when running (create-refman) again.
;;;; ---

;; some silence ...
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)
(declaim (sb-ext:muffle-conditions style-warning))

(defparameter weyl-folder "../")
(load (format nil "~A~A" weyl-folder "new/weyl-user-manual"))

(defparameter src-files (list
"packages"
"lisp-support"
"domain-support"
"classes/algebraic-domains"
"classes/space-classes"
"classes/general-classes"
"avl"
"lisp-numbers"
"sets"
"morphisms"
"quotient-fields"
"general" 
"fourier"
"functions"
"direct-sums"
"numbers/bigfloat"
"numbers/numbers"
"numbers/gfp"
"polynomials/poly-tools"
"polynomials/mpolynomial"
"polynomials/upolynomial"
"polynomials/epolynomial"
"polynomials/sparsegcd"
"polynomials/grobner"
"tpower"
"taylor"
"rational-functions"
"differential-domains"
"algebraic-extension"
"vector-spaces/vector"
"vector-spaces/projective-space"
"vector-spaces/quaternions"
 "matrix"
"topology"
"funct-spaces"
"mesh"
"new/weyl-infix"
"new/ge-support"
"new/ge-latex"))

 

(defun doc-output-file (f suffix)
  (if (find #\/ f)
    (setf f (subseq f (+ (position  #\/ f)1))))
      (format nil "~Adocs/~A.~A" weyl-folder f suffix))

(defun document-files (g &key (fmt 'text) (suffix "txt"))
  (loop for f in g do 
    (progn (format t "~A~%" f)
      (with-open-file (*standard-output* (doc-output-file f suffix)
        :direction :output
        :if-exists :supersede)
        (ignore-errors 
          (create-user-manual 
             (format nil "~A~A.lisp" weyl-folder f)   
                :output-format fmt))))))

;; (document-files src-files)
;; (document-files src-files :fmt 'latex :suffix "tex")
;; (document-files src-files :fmt 'text :suffix "md")

;; txt2html -p 0 


(defparameter html-index
"<html>
<head>
<title>Example for Frame</title>
</head>
    <frameset cols=\"30%,*\">
    <frame src=\"nav.html\" name=nav> 1st FRAME
    <frame src=\"start.html\" name=main> 2nd FRAME
    </frameset>
</html>")


(defparameter html-nav
"
<img src=\"data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAAUA
       AAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO
        9TXL0Y4OHwAAAABJRU5ErkJggg==\" alt=\"Red dot\"/> 
<pre>
<a href=\"start.html\"  target=\"main\">START</a>
</pre>

<pre>
<a href=\"../reference/Weyl Manual.pdf\"  target=\"main\">User Manual [PDF]</a>
</pre>

<pre>
SOURCE FILES
</pre>
<details>
<summary>Packages and Lisp support</summary>
<a href=\"packages.txt\"  target=\"main\">packages</a><br>
<a href=\"lisp-support.txt\"  target=\"main\">lisp-support</a><br>
<a href=\"domain-support.txt\"  target=\"main\">domain-support</a><br>
</details>

<details>
<summary>Classes</summary>
<a href=\"algebraic-domains.txt\"  target=\"main\">algebraic-domains</a><br>
<a href=\"space-classes.txt\"  target=\"main\">space-classes</a><br>
<a href=\"general-classes.txt\"  target=\"main\">general-classes</a><br>
</details>

<details>
<summary>Generic Tools</summary>
<a href=\"avl.txt\"  target=\"main\">avl</a><br>
<a href=\"lisp-numbers.txt\"  target=\"main\">lisp-numbers</a><br>
</details>

<details>
<summary>Basic Tools</summary>
<a href=\"sets.txt\"  target=\"main\">sets</a><br>
<a href=\"morphisms.txt\"  target=\"main\">morphisms</a><br>
<a href=\"quotient-fields.txt\"  target=\"main\">quotient-fields</a><br>
</details>

<details>
<summary>General Expressions</summary>
<a href=\"general.txt\"  target=\"main\"> general</a><br>
</details>

<details>
<summary>Sums, Product, Quotients</summary>
<a href=\"fourier.txt\"  target=\"main\">fourier</a><br>
<a href=\"functions.txt\"  target=\"main\">functions</a><br>
<a href=\"direct-sums.txt\"  target=\"main\">direct-sums</a><br>
</details>

<details>
<summary>Numbers</summary>
<a href=\"bigfloat.txt\"  target=\"main\">bigfloat</a><br>
<a href=\"numbers.txt\"  target=\"main\">numbers</a><br>
<a href=\"gfp.txt\"  target=\"main\">gfp</a><br>
</details>

<details>
<summary>Polynomials</summary>
<a href=\"poly-tools.txt\"  target=\"main\">poly-tools</a><br>
<a href=\"mpolynomial.txt\"  target=\"main\">mpolynomial</a><br>
<a href=\"upolynomial.txt\"  target=\"main\">upolynomial</a><br>
<a href=\"epolynomial.txt\"  target=\"main\">epolynomial</a><br>
<a href=\"sparsegcd.txt\"  target=\"main\">sparsegcd</a><br>
<a href=\"grobner.txt\"  target=\"main\">grobner</a><br>
</details>

<details>
<summary>Truncated Power Series</summary>
<a href=\"tpower.txt\"  target=\"main\">tpower</a><br>
<a href=\"taylor.txt\"  target=\"main\">taylor</a><br>
</details>

<details>
<summary>Algebraic structures</summary>
<a href=\"rational-functions.txt\"  target=\"main\">rational-functions</a><br>
<a href=\"differential-domains.txt\"  target=\"main\">differential-domains</a><br>
<a href=\"algebraic-extension.txt\"  target=\"main\">algebraic-extension</a><br>
</details>

<details>
<summary>Linear Spaces</summary>
<a href=\"vector.txt\"  target=\"main\">vector</a><br>
<a href=\"projective-space.txt\"  target=\"main\">projective-space</a><br>
<a href=\"quaternions.txt\"  target=\"main\">quaternions</a><br>
<a href=\"matrix.txt\"  target=\"main\">matrix</a><br>
</details>

<details>
<summary>Topology and Function Spaces</summary>
<a href=\"topology.txt\"  target=\"main\">topology</a><br>
<a href=\"funct-spaces.txt\"  target=\"main\">funct-spaces</a><br>
</details>

<details>
<summary>Meshing</summary>
<a href=\"mesh.txt\"  target=\"main\">mesh</a><br>
</details>

<details>
<summary>New files</summary>
<a href=\"weyl-infix.txt\"  target=\"main\">weyl-infix</a><br>
<a href=\"ge-support.txt\"  target=\"main\">ge-support</a><br>
<a href=\"ge-latex.txt\"  target=\"main\">ge-latex</a><br>
</details>

<pre>
EXAMPLES
</pre>
")


(defparameter html-start
"<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\"/>
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>The Weyl Computer Algebra Substrate</title>
</head>
<body>
  <header>
<img src=\"data:image/png;base64, 
iVBORw0KGgoAAAANSUhEUgAAAPQAAAGaCAMAAAAPeJ5SAAAAw3pUWHRSYXcg
cHJvZmlsZSB0eXBlIGV4aWYAAHjabVDREcMgCP1nio6goAbGIU1y1w06flFI
LzZ9noA8fCKwv18HPDowFyh14SatJUORIqgWcHLosDmVYQeOElye8yAaIZon
8+QEt6g/8/kr4K7fqxchfgaxzoTEy8g/QuiOekc93s6OQojQiRwC6t9KTXi5
fmHd0wz2Dd0Untu+nReb3lbtHULcKVMyS9S8Aeq7AqkFNKz0QltK1YrVshJi
NpB/czoBH38/WYZPu0mnAAABhGlDQ1BJQ0MgcHJvZmlsZQAAeJx9kT1Iw0Ac
xV9bpSoVByuIiGSoTnapIo61CkWoEGqFVh1MLv2CJg1Jiouj4Fpw8GOx6uDi
rKuDqyAIfoC4C06KLlLi/5JCixgPjvvx7t7j7h3gb1SYanbFAVWzjHQyIWRz
q0LwFQH0YggxjEnM1OdEMQXP8XUPH1/vojzL+9yfo1/JmwzwCcRxphsW8Qbx
zKalc94nDrOSpBCfE08adEHiR67LLr9xLjrs55lhI5OeJw4TC8UOljuYlQyV
eJo4oqga5fuzLiuctzirlRpr3ZO/MJTXVpa5TnMUSSxiCSIEyKihjAosRGnV
SDGRpv2Eh3/E8YvkkslVBiPHAqpQITl+8D/43a1ZmIq5SaEE0P1i2x/jQHAX
aNZt+/vYtpsnQOAZuNLa/moDmP0kvd7WIkfAwDZwcd3W5D3gcgcYftIlQ3Kk
AE1/oQC8n9E35YDBW6Bvze2ttY/TByBDXaVugINDYKJI2ese7+7p7O3fM63+
fgC6ZXLDbflT9QAADXhpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBh
Y2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlk
Ij8+Cjx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1w
dGs9IlhNUCBDb3JlIDQuNC4wLUV4aXYyIj4KIDxyZGY6UkRGIHhtbG5zOnJk
Zj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5z
IyI+CiAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgIHhtbG5z
OnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAg
eG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlw
ZS9SZXNvdXJjZUV2ZW50IyIKICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5v
cmcvZGMvZWxlbWVudHMvMS4xLyIKICAgIHhtbG5zOkdJTVA9Imh0dHA6Ly93
d3cuZ2ltcC5vcmcveG1wLyIKICAgIHhtbG5zOnRpZmY9Imh0dHA6Ly9ucy5h
ZG9iZS5jb20vdGlmZi8xLjAvIgogICAgeG1sbnM6eG1wPSJodHRwOi8vbnMu
YWRvYmUuY29tL3hhcC8xLjAvIgogICB4bXBNTTpEb2N1bWVudElEPSJnaW1w
OmRvY2lkOmdpbXA6ZDBiMDE2YzAtODBjOS00ZmNlLWFjODMtNjdlNjNiMGFj
NDIwIgogICB4bXBNTTpJbnN0YW5jZUlEPSJ4bXAuaWlkOmE1ZTNiMjhjLTBj
ZDMtNDQ4ZC04NzA2LWVhNzEwMTJiZjZjZSIKICAgeG1wTU06T3JpZ2luYWxE
b2N1bWVudElEPSJ4bXAuZGlkOjE5YTliZjYyLTJkY2EtNDNhYS1iOWMxLTE4
NjE3YzU1OWU2ZSIKICAgZGM6Rm9ybWF0PSJpbWFnZS9wbmciCiAgIEdJTVA6
QVBJPSIyLjAiCiAgIEdJTVA6UGxhdGZvcm09IkxpbnV4IgogICBHSU1QOlRp
bWVTdGFtcD0iMTc0MTU1NzEwMDY3MzY5MyIKICAgR0lNUDpWZXJzaW9uPSIy
LjEwLjM2IgogICB0aWZmOk9yaWVudGF0aW9uPSIxIgogICB4bXA6Q3JlYXRv
clRvb2w9IkdJTVAgMi4xMCIKICAgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyNTow
MzowOVQyMjo1MTozOSswMTowMCIKICAgeG1wOk1vZGlmeURhdGU9IjIwMjU6
MDM6MDlUMjI6NTE6MzkrMDE6MDAiPgogICA8eG1wTU06SGlzdG9yeT4KICAg
IDxyZGY6U2VxPgogICAgIDxyZGY6bGkKICAgICAgc3RFdnQ6YWN0aW9uPSJz
YXZlZCIKICAgICAgc3RFdnQ6Y2hhbmdlZD0iLyIKICAgICAgc3RFdnQ6aW5z
dGFuY2VJRD0ieG1wLmlpZDo2NzBlNGRhOS0wYjk2LTRmNzctOGI1ZS0xZWI0
OGU5Y2VhMzYiCiAgICAgIHN0RXZ0OnNvZnR3YXJlQWdlbnQ9IkdpbXAgMi4x
MCAoTGludXgpIgogICAgICBzdEV2dDp3aGVuPSIyMDI1LTAzLTA5VDIyOjUx
OjQwKzAxOjAwIi8+CiAgICA8L3JkZjpTZXE+CiAgIDwveG1wTU06SGlzdG9y
eT4KICA8L3JkZjpEZXNjcmlwdGlvbj4KIDwvcmRmOlJERj4KPC94OnhtcG1l
dGE+CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
IAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
CiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAK
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAg
ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAog
ICAgICAgICAgICAgICAgICAgICAgICAgICAKPD94cGFja2V0IGVuZD0idyI/
PiZks8MAAABgUExURUlJUP//////7//65P/x2/zn0fPeyerVwd/LuNXCr8u5
p8Cwn7Wmlaqci6GTg5iKepCCcoh7aoFzYnlrW3NkUWRiZGpbS2FRQ1VTWFdH
PkxIRUtAOkJBRD8/Pz09PD07NkAb8LgAAAAJcEhZcwAACxMAAAsTAQCanBgA
AAAHdElNRQfpAwkVMygcBHnhAAAgAElEQVR42t1diWKbPLN143jF7AQsCfS/
/1veOWckwEsSOwnp/Urb1NlsH2Y0+7J6q+s3XuH/8Jlc5/jhrNfbeYUP4yer
8+0lX8QffMDV86O1q3Cdwwf9QcunWIVPz/qb+m/8xnl6lfHRW/jsLbw3fZvx
7c6/fHO1b1VZlqvx8/riu2f8GSFevQEb3lOEoY/Os08C4qHnNfSr3uoNmN2B
Xh7a6Wf1A/Ha3vJT/Kz8jLXhifG6fG2rj/Ho/P71phguUdd1Va/u3Y+38/z2
rvp+JNoIS163t5cg8UNyyQfLj0A79PiDB7aPNwCPbHho8XcVf1y+zsf4Kq6e
98Xq6+v9sbh75x4vtbJKjPDGzhOXBCKMZArkJ+hWqLu6ou+b0jc8ReRSfV96
swO9wpsEpBXfc3zfI9ohfND/+EX56Pv4KN4PvTWWjwE6fD08v+WXeaPsSj/F
i5FxzrwnNnDMOby9Pt6giDxyaGDwt0Dp+KX50bykX3gbik4h++nrI+CAbby8
l3/DxVf0q4EJxjsTfi/ernBjhvEODcos+JoPd4x3hN8kO4ycZuX+WX2jwNGv
xmMJqnZtA0qHkyv/R/qu4hFaRSR4sxMN9T14vj+B7sMXFFEfUHqFjI9ev9SH
LyjwIXIBP+vDN3x42vG7Qz/+ir6e9/Hr8UbNCIKTYMdPBj0mI/+Di9uuFdCj
mLpErEzC55mo1U9vxPv4Bf8+Wcdv+PC9QOdLwl98qjfE3zwPbk4fvqM/E08R
7zwfTHIj3i18yQa2DxDlVEeha1fhCPN88OzYO+w6e5Peh7/h5t/Hexfa41d4
Yu/v3Csy2HiveF/7y7erUgHCoLdBvwhjK+hRgUwSajUTSSN7Xr2TYaTdY+//
G8j9HbL7+Xf7fjxcF+9whGFtZHGKaTIx0a7sDGt84D8lxO9egej++vUVZz+/
vxPdrAo7VepvLQyjoId7FQGzk4EP/p37Pp3TZaDded1LGXdPIFxIPR9lbjQB
5FJjhRw9KDufowzoo6aY5OzNm/h9Ivfe3+H0S3JfnXvf+z4qQqtWr4jwlgLa
E6Iw9zCqn37SLP7/CV9fovK3AsPffVt9UPPU4EE1r4ZJ26lItEEJxxPyX7j8
fZE52g0q2COxxY4dLaP++on694nq/3v3ou+jZQoRFr9vJ6PAX5gK/3+BPPTT
3s/EefBhV9Et6P2jr+CH/94V9NIquo4QjMFA9P8AvHunNLoOapipga2ez7V2
/O8jvrJJ+1WIf9h+0vj+F0yv/i+JgWCmaHBqNXMLf4W03nvv/x69GZ2T030p
s7/nHTxiPP8+aLxoP8pvWqF3DOylbOredF3bGbeckvrwKRCjPCto/0snyznT
tW3byWXuqYqlBYra2PAwrfuNsywQBTDir1UN2Ma5K4byP++I31fWDJbY35Cn
Vri6qepGWNsIYPlnZkDfcVKD8fCjJAkxol+Q2q4DdVv8aVpjANx07jK69h6N
5Z/7wSM2BNALHGp/xdktL4FeV02nmI359GX1XjjTds7/3KkOxsnS8gsIHYAK
8AYc7m5O9AeUlt9s52zxPRMh6GnbDz8f97lU/Q6Kqm3qqqrlahr5IND9YzIK
v9x1zn3/TUJfEDSskyUdQMoovG+89bYFvamyoL7sQzBEtxvT1A20u/8ObpUQ
aobaT37Q/9AdIE/jHjgwOsjePGSkyK/ITRJtx4PxHdhkrJCL+8gH+CHGD6FT
EWkgNQCL+npYPHknQqFtyqqpu29K8nCm1bH8/HR+n9xyoJxzirpp26pqzYNP
7rvWmaYpCyH39wS5j3UC/fBrDgBtTyBvIcweJ5s/V53cqLIqy/p7+kvjCL9i
kI3hdwHsPHRXZyw+s/j8c6PLD00l/NGWZV6U7bdQa+GH7fthWVqDpUlkp1YK
zBNnLfWv4Bbsn994K6dBzPa6yoqqdV+86yQ1QbvFSW1V6ArkuiphncA2k/9p
pODvpwZ2L79dV11V1G1T5IXQ2n8RdLTI+kV4+Up4w8ZooWzlAvZKGLzDDWjk
nIpt+oAtXDemLOC0CMW/LMRVeq9+HPTNWfHka6hZZXVjLKS4uF1ioYlEg91h
Pj+ormptm5eCWAycr55HCjKe6YWFt3MdzYpBoQmr2wGeZtME0sPW8p/fSlsb
V2VlhXv35UMZqrfs4sLbqa6aGeQe7mbDiIKxVo21zznIihlbF2XtfP9lDyQU
SS1LaH/rf6jjJdTH4S4rsc8YNfOf63j8GqT/O/njx2Inq1laZ1EVfes54YyL
C1IVeZ6dsrI1D0Yj6HE5xztn/RcP9cr2v57KoDFq1eFC4CzP86LuHgnhuK6R
XxF3walJ/gU7VFB36k/7JYF7fxMCIn8jhCTCuyqLoszzunuEcnIkWg01eQrE
p7lOK1D7fvjNtFWo/HH0PZTQYlAL8DQTdW0+NUmNWOBdEN7++RcPZ9r2vwn3
Io5EES52ac2rzDJ4E/6TJzFNIWfBfJHnQjrrSePEfwe0o0qOrqacSwg02KX4
C9+r7T4V4oNr86xovhrICQXWv5ZHFMOsn841tDX0FuNnAh22+EN31Jsmz/LG
fU20aIHsk8aJ919n68tUjpdTzXM9Uts89OygtQj88mnH2keVdbb2V4+yWmY+
yDJKcCFyVVU0SB+KZ+BgdE2ZV91Xolma2PndLLkcqhA6YFy4URlGz6MNGa5H
nsR1VU7H+mm+0xTeL5cGdJrNok0Gt7KETSbGSVEy0fXgsRYVXcvBrs0XDtuo
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
S2s7Rdys9a2BZi9Snf8caX1XW8XXwBRORn/DQEKC7r6hp2cHOqDOImYlNWpX
MQX17nsyLIaVu6O7c2hvsUoM/VNIY6TJKa/bmQ8iAl/UdFbEGdAhFZJU7+fA
a414h9YORb3Z269T2lxK7mlNSDjV3NjEY30PNbvQGATVJkqUurGAHfIRzhs6
U05ZWmMgUIhhi7vVyVlPktnka3TPv1ewStCqsDavcTzwZnOwX6e0u/I0JtBK
acrvwOA3cWF4UDU7o3UXAXU75gu2FGfcg14w0JuPY/Y9MkgiNMPwa+aAOCbh
XdDQ8KeR0nGv0vG5SpELf3rowooQBAPnC2FUVesiUdZ+mbtT59BlGYQ9vA78
Rsn5aw3mmaClpeJ2mZPqsTESkrHiINHGYZQ2+w/YW8uvQGkd644QU/JE0ZvH
dJf+IhI7auiZEMsDaD3UCtrc20Jnwx7xUClC5tAeDVyGASXyf1E3wVJznbOQ
cGGfKdova/+BeQ8NcRpBhzHJs0a0523vwbdRcM9BZzP53Yyg7+lro3v9hBZh
BKzWdKMuA7rLaQtCjgEBXSD0YCuFrC17yakcPgJdFwp6N4EW1E+Bvp6J4C0i
Yzp9PM9G0X3J3ySzfLD33MtWd9wpu55YAcey7iAOuFMeUQDE0DicrM2TYxKH
Wcmj/APrHuXkMaETXSxGBg/fAS2knpj7ArKCzqvQccEijVsfyIl4ZUYxjixK
aLUXbLNr+aucuBgGAolfjdKRJC6pQLl/90m+DIlBxvm3iBSFeZrfkd44ltUo
xOL2yWz8mAf7G6Hc+wM0GUggMZKxdiK0Iqn6Im7UCPK0a1wTk5BDKjv7QIih
TIJCI9U6QUpvtLpsNvunjZOLV/HdZH7mE9yIuqyizrpvoHgThFVyGstkjuxF
wqmpar3KPBR9FrpZfiR0WvtPgjNTh84mWt6oYGifFWRXA5uakcZXoKdDrdU4
998fhhuM7a86BQIl7TrUhZaP9vIgQZ/lQdDriU5CYd2noDkumavEYn/E1/3p
oESquSUWtpilClqVlnP98B5mP7SmjbMv4t4cbOkjm5DgWdisrmMeqNJPHI+Q
1p8X89D2TrTuYhobu6ueTOD5/1196aylt6nuS56ZodS7Gvpy7h0hixoSU1PY
xFY0re4FZXXnX1gnfwqzLXRzFgR99XnqyIQSm13cYRF2NJRPgr5lqCoLpE5H
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
eN3y/wA65PFsc1SaQAAAAABJRU5ErkJggg==\" alt=\"Hermann Weyl\"/> 

<br>
<a href=\"https://en.wikipedia.org/wiki/Hermann_Weyl\">
  Hermann Weyl 1885-1955</a>
<br>

    <h5>References</h5>
  </header>
  <nav>
    <ul>
      <li><a href=\"https://www.cs.cornell.edu/Info/Projects/SimLab/releases/release-1-0.html\">
         SimLab (Cornell)</a></li>
      <li><a href=\"https://www.cs.cornell.edu/Info/Projects/SimLab/projects/computer-algebra.html\">
         Richard Zippel</a></li>
      <li><a href=\"https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/tools/0.html\">
         Lisp tools at CMU (Mark Kantrowitz)</a></li>
    </ul>
  </nav>
  <main>
   <h4>This is h4</h4>
   <pre> This is -- preformatted </pre>
  </main>
  <footer>
     <h5>Meta</h5>
     <a href=\"contact.html\">Contact</a>
     <p>created: 10-MAR-2025</p>
  </footer>
</body>
</html>")


(defun create-html-page (fn tpl)
  (with-open-file (*standard-output* (doc-output-file fn "html")
    :direction :output
    :if-exists :supersede)
    (ignore-errors (format t "~A" tpl))))
    

; (create-html-page "index" html-index)
; (create-html-page "nav" html-nav)
; #!/bin/sh
; for a in *.txt; do txt2html -p 1 $a > $(basename $a .txt).html ; done

(defun create-refman ()
  (progn
    (document-files src-files)
    (create-html-page "index" html-index)
    (create-html-page "nav" html-nav)
    (create-html-page "start" html-start)))

;;START HERE: (create-refman)  creates the reference manual in weyl/docs