;; phaedra-dft-test.lisp

(in-package :phaedra-dft)

(defparameter *passed* nil)
(defparameter *failed* nil)
(defparameter *results* nil)
(defparameter *groups* nil)

(defun init-testing ()
  (setf *groups* nil)
  (setf *results* nil))

(defun in-group (group-symbol)
  (push nil *results*)
  (push group-symbol *groups*))

(defmacro test (name lhs comparison rhs)
  `(progn 
     (push
      (list
       ',name 
       (lambda () 
	 (if (funcall #',comparison ,rhs ,lhs)
	     (progn (incf *passed*)
		    (format t "ok: ~a~%" ',name))
	     (progn (incf *failed*)
		    (format
		     t
		     "NOT OK: ~a ==> lhs:~a rhs:~a~%"
		     ',name
		     ,lhs
		     ,rhs)))))
      (car *results*))
     nil))

(defun run-tests ()
  (setf *passed* 0)
  (setf *failed* 0)
  (loop 
     for test-pair-group in (reverse *results*)
     and group in (reverse *groups*)  
     do
       (terpri)
       (format t "~&~a~%" group)
       (loop for test-pair in test-pair-group
	  do 
	    (format t "   ")
	    (funcall (cadr test-pair))))
  (terpri)
  (format t "~&passed: ~a" *passed*)
  (format t "~&failed: ~a" *failed*)
  nil)

;; --------------------------------------------------

(init-testing)

(in-group 'roots-of-unity)

(let ((n 10) (j 4))
  (test root-symmetry-1
	(expt (conjugate (first-root-of-unity n)) j)
	complex-near-p
	(root-of-unity (- n j) n)))

(let ((n 9) (j 7))
  (test root-symmetry-2
	(expt (conjugate (first-root-of-unity n)) j)
	complex-near-p
	(root-of-unity (- n j) n)))

(let ((n 15))
  (test root-sum-powers-1
	(loop for k from 0 to (- n 1)
	      summing (expt (first-root-of-unity n) k))
	complex-near-p
	0.0))

(let ((n 15))
  (test root-sum-powers-2
	(loop for k from 0 to (- n 1)
	      summing (root-of-unity k n))
	complex-near-p
	0.0))

(let ((n 15))
  (test root-power-n-propery
	(loop for k from 0 to (- n 1)
	      always (complex-near-p
		      (expt (root-of-unity k n) n)
		      #C(1.0 0.0)))
	equal
	t))

(in-group 'polygons-and-matrices)

(let ((poly (first-fourier-polygon 10)))
  (test fourier-dot-product
	(matrix-mul
	 (matrix-transpose-conjugate poly)
	 poly)
	complex-near-p
	10.0))

(let ((fmatrix (fourier-matrix 10)))
  (test fourier-matrix-inverse
	(matrix-mul
	 (matrix-transpose-conjugate fmatrix)
	 fmatrix)
	matrix-near-p
	(make-diagonal-matrix
	 (make-list 10 :initial-element 10))))

(let* ((n 7)
       (fmatrix (fourier-matrix n)))
  (test fourier-matrix-columns-are-polygons
	(loop for col from 0 below n 
	      always (matrix-near-p
		      (matrix-get-column fmatrix col)
		      (fourier-polygon col n)))
	equal
	t))
