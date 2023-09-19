;;; phaedra-dft.lisp

(in-package #:phaedra-dft)

;; A polygon is a cyclic vector of complex numbers of length N.

(deftype complex-vertex () '(complex double-float))

(defconstant +i+ #C(0 1))

;; The first nth-root of unity.
;; The 0th nth root of unity is always 1.

(defun first-root-of-unity (n)
  (exp (/ (* 2 pi +i+) n)))

;; the kth nth-root of unity is the kth
;; power of the first nth-root.

(defun root-of-unity (k n)
  (exp (/ (* 2 pi +i+ k) n)))

(defun pref (polygon k)
  (aref polygon (mod k (length polygon))))

(defun make-polygon (n)
  (make-array n
	      :fill-pointer 0
	      :adjustable t
	      :element-type 'complex-vertex))

;; the 1st primitive polygon of order n.
(defun fundamental-primitive-polygon (n)
  (loop for j from 0 to (- n 1)
	with fpp = (make-polygon n)
	do (vector-push (root-of-unity j n) fpp)
	finally (return  fpp)))

;; (kth primitive polygon of order n.
(defun primitive-polygon (k n)
  (map 'vector
       #'(lambda (w) (expt w k))
       (fundamental-primitive-polygon n)))

(defun write-polygon-vertices (polygon filename)
  (with-open-file
      (out filename :direction :output :if-exists :supersede)
    (format out "~a~%" (length polygon))
    (loop for w across polygon
	  and vertex-label from 0
	  do (format out "~a ~a ~a ~%"
		     (realpart w) (imagpart w) vertex-label))))

(defun try-it ()
  (let ((poly (primitive-polygon 12 44)))
    (write-polygon-vertices poly "polygon.dat")))    
