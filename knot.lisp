(defpackage :dls
  (:use :common-lisp)
  (:export :knot-round :knot-hash))

(in-package :dls)

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun byte->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (let* ((blist (integer->bit-list integer))
	   (blen (length blist)))
      (if (< blen 8)
	  (setf blist (append (loop for i below (- 8 blen) collect 0) blist)))
      (coerce blist 'bit-vector))))

;; modifies buf
;; buf: vector
;; n, pos: 0..(1- (length buf))
(defun reverse-n-at (buf n pos)
  (let ((chunk (reverse (subseq (concatenate 'vector buf buf) pos (+ n pos)))))
    (dotimes (i n)
      (setf (elt buf (mod (+ pos i) (length buf))) (elt chunk i))))
  buf)

(defun dump-round (rnum inp)
  (format t "round ~A:~%" rnum)
  (dotimes (i 16)
    (dotimes (j 16)
      (format t " ~d" (elt inp (+ (* 16 i) j))))
      (format t "~%"))
      (format t "~%"))

(let ((skip 0)
      (pos 0))

  ;; This is a round of the 'knot hash'
  (defun knot-round (lengths input)
    (loop for l across lengths do
	 (progn
	   (reverse-n-at input l pos)
	   (incf pos (+ l skip))
	   (setf pos (mod pos (length input)))
	   (incf skip)))
    input)

  ;; str is a string
  (defun knot-hash (str)
    ;; Input string becomes the 'lengths' array
    (let ((lens (make-array '(0) :fill-pointer 0 :adjustable t))
	  (inp (apply #'vector (loop for i below 256 collect i))))
      (loop for c across str do
        (vector-push-extend (char-code c) lens))
      (dolist (n '(17 31 73 47 23))
	(vector-push-extend n lens))
      ;; (format t "lens: ~A~%" (apply #'concatenate 'string (loop for i across lens collect (format nil " ~d" i))))

      ;; Do 64 rounds of knot-hash on the canonical input using the given input lengths
      (setf skip 0)
      (setf pos 0)
      (dotimes (rnum 64)
	;; (dump-round rnum inp)
	(knot-round lens inp))

      ;; Convert the 256-byte hashed input to 16-byte dense hash
      (let ((dense-hash (make-array '(16) :fill-pointer 0 :adjustable t)))
	(loop for i below 256 by 16
	   do (let ((val (elt inp i)))
		(loop for j from (1+ i) below (+ i 16)
		   do (let ((bv-val (byte->bit-vector val))
			    (bv-arg (byte->bit-vector (elt inp j))))
			(setf val (bit-vector->integer (bit-xor bv-val bv-arg)))))
		(vector-push-extend val dense-hash)))

	(let ((strhash (apply #'concatenate 'string
			      (loop for i across dense-hash
				 collect (format nil "~2,'0x" i)))))
	  (values strhash dense-hash))))))
