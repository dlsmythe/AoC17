;;; New here:
;;; - with-input-from-string
;;; - process a file a line at a time (without the loop macro)

(defun parse-vals (str)
  (with-input-from-string (s str) (loop for x = (read s nil nil) while x collect x)))

(with-open-file (in "adv17-02.input")
  (let ((cksum1 0)
	(cksum2 0))
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (let ((vals (parse-vals line)))
	(let ((minval most-positive-fixnum)
	      (maxval most-negative-fixnum)
	      (numerator 0)
	      (divisor 0)
	      (numvals (length vals)))
	  (do* ((i 0 (1+ i))
		(v (nth i vals) (nth i vals)))
	       ((>= i numvals))
	    (if (> v maxval)
		(setf maxval v))
	    (if (< v minval)
		(setf minval v))
	    (do ((j 0 (1+ j)))
		((or (>= j numvals) (/= 0 divisor)))
	      (unless (= i j)
		(let ((v2 (nth j vals)))
		  (cond ((and  (/= 0 v2) (= 0 (mod v v2)))
			 (setf numerator i)
			 (setf divisor j))

			;; NB: This test is unnecessary - it would be performed
			;;      eventually in any case.  This is a just an optimization.
			((and (/= 0 v) (= 0 (mod v2 v)))
			 (setf numerator j)
			 (setf divisor i)))))))

	      (incf cksum1 (- maxval minval))
	      (incf cksum2 (/ (elt vals numerator) (elt vals divisor))))))
    (format t "part 1 cksum is ~A~%" cksum1)
    (format t "part 2 cksum is ~A~%" cksum2)))

(sb-ext:exit :code 0)
