;;; phaedra-dft.lisp

(in-package #:phaedra-dft)

(defconstant +i+ #C(0 1))
(defconstant +complex-r-tolerance+ 1.0d-12)
(defconstant +complex-arg-tolerance+ 1.0d-3)
(defconstant +complex-tolerance+ 1.0d-10)

(defun complex-near-p (z1 z2)
  (let ((x1 (realpart z1))
	(y1 (imagpart z1))
	(x2 (realpart z2))
	(y2 (imagpart z2)))
    (and (< (abs (- x1 x2)) +complex-tolerance+)
	 (< (abs (- y1 y2)) +complex-tolerance+))))

(defun complex-near-zero-p (z)
  (< (abs z) +complex-tolerance+))

;; Check that all elements of mat1 and mat2 are close.

(defun matrix-near-p (mat1 mat2)
  (if (equal (array-dimensions mat1)
	     (array-dimensions mat2))
      (destructuring-bind (rows cols)
	  (array-dimensions mat1)
	(loop for row from 0 below rows
	      always
	      (loop for col from 0 below cols
		    always
		    (complex-near-p (aref mat1 row col)
				    (aref mat2 row col)))))
      (error "~a, ~a comparing matrices of different shapes"
	     (array-dimensions mat1)
	     (array-dimensions mat2))))

;; The first nth-root of unity.
;; The 0th nth root of unity is always 1.

(defun first-root-of-unity (n)
  (exp (/ (* 2 pi +i+) n)))

;; the kth nth-root of unity is the kth
;; power of the first nth-root.

(defun root-of-unity (k n)
  (exp (/ (* 2 pi +i+ k) n)))

;; Matrices are m x n with double float complex elements.
;; Row vectors are 1 x n complex matrices.
;; Column vectors are m x 1 complex matrices.

(deftype matrix-element () '(complex double-float))

(defun make-matrix (rows cols)
  (make-array
   (list rows cols)
   :element-type 'matrix-element))

(defun make-column-vector (rows) (make-matrix rows 1))

(defun make-row-vector (cols) (make-matrix 1 cols))

(defun matrix-conjugate (matrix)
  (destructuring-bind (rows cols)
      (array-dimensions matrix)
    (let ((cmatrix (make-matrix rows cols)))
      (loop for row from 0 below rows do
	(loop for col from 0 below cols do
	  (setf (aref cmatrix row col)
		(conjugate (aref matrix row col)))))
      cmatrix)))

(defun matrix-transpose (matrix)
  (destructuring-bind (rows cols)
      (array-dimensions matrix)
    (let ((tmatrix (make-matrix cols rows)))
      (loop for row from 0 below rows do
	(loop for col from 0 below cols do
	  (setf (aref tmatrix col row)
		(aref matrix row col))))
      tmatrix)))

(defun matrix-transpose-conjugate (matrix)
  (matrix-conjugate (matrix-transpose matrix)))

(defun matrix-trace (matrix)
  (let ((m (apply #'min (array-dimensions matrix))))
    (loop for i from 0 below m
	  summing (aref matrix i i))))

;; Returns x rather than [x] if result should be a scalar

(defun matrix-mul (amatrix bmatrix)
  (destructuring-bind (arows acols)
      (array-dimensions amatrix)
    (destructuring-bind (brows bcols)
	(array-dimensions bmatrix)
      (assert (= acols brows))
      (let ((result (make-matrix arows bcols)))
	(loop for row from 0 below arows do
	  (loop for col from 0 below bcols do	    
	    (setf (aref result row col)  
		  (loop for i from 0 below acols
			summing (* (aref amatrix row i)
				   (aref bmatrix i col))))))
	(if (= arows bcols 1)
	    (aref result 0 0)
	    result)))))

(defun matrix-get-row (matrix row)
  (destructuring-bind (_ cols)
      (array-dimensions matrix)
    (declare (ignore _))  
    (loop for col from 0 below cols
	  with result = (make-row-vector cols)
	  do (setf (aref result 0 col)
		   (aref matrix row col))
	  finally (return result))))

(defun matrix-get-column (matrix col)
  (destructuring-bind (rows _)
      (array-dimensions matrix)
    (declare (ignore _))
    (loop for row from 0 below rows
	  with result = (make-column-vector rows)
	  do (setf (aref result row 0)
		   (aref matrix row col))
	  finally (return result))))

(defun matrix-print (matrix)
  (destructuring-bind (rows cols)
      (array-dimensions matrix)
    (loop for row below rows do
      (format t
	      "~{~,3@f~,3@fi~^ ~}"
	      (loop for col below cols
		    nconc
		    (let ((z (aref matrix row col)))
		      (list (realpart z) (imagpart z)))))
      (terpri))))

;; Make a square diagonal matrix.
;; Supply the diagonal in the form of a list.

(defun make-diagonal-matrix (diagonal)
  (let* ((n (length diagonal))
	 (result (make-matrix n n)))
    (loop for row from 0 below n
	  and d in diagonal do
      (loop for col from 0 below n do
	(if (= row col)
	    (setf (aref result row col)
		  (coerce d 'complex-element))
	    (setf (aref result row col)
		  #C(0.0 0.0)))))
    result))

;; A polygon is a cyclic vector of complex numbers of length N.
;; Usually polygons are defined as column vectors.

(defun pref (polygon k)
  (destructuring-bind (rows cols)
      (array-dimensions polygon)
    (cond ((= rows 1)
	   (aref polygon 0 (mod k cols)))
	  ((= cols 1)
	   (aref polygon (mod k rows) 0))
	  (t
	   (error "~s ~s bad polygon dimensions" rows cols)))))

(defun make-polygon (n)
  (make-column-vector n))

;; The 1st Fourier polygon of order n.

(defun first-fourier-polygon (n)
  (loop for j from 0 below n
	with result = (make-polygon n)
	do (setf (aref result j 0)
		 (root-of-unity j n))
	finally (return  result)))

;; The kth Fourier polygon of order n.

(defun fourier-polygon (k n)
  (loop for j from 0 below n
	with result = (make-polygon n)
	do (setf (aref result j 0)
		 (expt (root-of-unity j n) k))
	finally (return result)))

;; Fourier matrix of order n is the n x n matrix
;; of kth n-order polygon columns with k = 0...n-1.

(defun fourier-matrix (n)
  (let ((result (make-matrix n n))
	(ffp (first-fourier-polygon n)))
    (loop for row from 0 below n do
      (loop for col from 0 below n do
	(setf (aref result row col)
	      (expt (aref ffp row 0) col))))
    result))


;; Write a list of polygons to file.
;; We assume polygon is in standard column form.

(defun write-polygons
    (polygon-list &optional (filename "polygon.dat"))
  (with-open-file
      (out filename :direction :output :if-exists :supersede)
    (format out "~a~%" (length polygon-list))
    (loop for polygon in polygon-list
	  do (destructuring-bind (rows _)
		 (array-dimensions polygon)
	       (declare (ignore _))
	       (format out "~a~%" rows)
	       (loop for row from 0 below rows
		     and vertex-label from 0
		     do (let ((z (aref polygon row 0)))
			  (format out
				  "~a ~a ~a ~%"
				  (realpart z)
				  (imagpart z)
				  vertex-label)))))))

;; Something more specialized.

(defun write-fourier-polygons
    (k-n-list &optional (filename "polygon.dat"))
  (write-polygons
   (mapcar #'(lambda (k-n-pair)
	       (apply #'fourier-polygon k-n-pair))
	   k-n-list)
   filename))    





(defun do-it ()
  (let ((foo (fourier-matrix 12)))
    (write-polygons
     (mapcar #'(lambda (u)
		 (matrix-get-column foo u))
	     '(0 1 2 3 4 5 6 7 8 9 10 11)))))

