;;; Run with: sbcl --noinform --load adv17-3.lisp
;;;  options:
;;;   -s <numshells>
;;;   -v <targetvalue>

(ql:quickload "getopt" :silent t)
(defparameter *max-shells* 6)
(defparameter *target-value* 368078)
(defparameter *table* (make-array '(1) :fill-pointer 0 :adjustable t))

(defun cell-value-at-coords (x y)
  (do ((i 0 (1+ i))
       (sum 0))
      ((or (/= 0 sum) (>= i (length *table*))) sum)
    (let ((val (aref *table* i)))
      (if (and (= x (first val))
	       (= y (second val)))
	  (setf sum (third val))))))
  
(defun next-larger (target-value)
  (let ((dir-incr (make-hash-table)))
    ;; third element is the direction of a left-turn from the given direction
    (setf (gethash 'EAST  dir-incr) '(1 0  NORTH))
    (setf (gethash 'NORTH dir-incr) '(0 1  WEST))
    (setf (gethash 'WEST  dir-incr) '(-1 0 SOUTH))
    (setf (gethash 'SOUTH dir-incr) '(0 -1 EAST))

    ;; First, determine the coordinates of each cell of the spiral
    (vector-push-extend (list 0 0 1) *table*)
    (let ((x 1)
	  (y 0))
      (do ((shell 1 (1+ shell)))
	  ((> shell *max-shells*))
	(let ((direction 'NORTH))
	  (loop for i from 0 to (1- (* 8 shell))
	     do (progn
		  (vector-push-extend (list x y 0) *table*)
		  (if (and (= 0 (mod (1+ i) (* 2 shell)))
			   (/= i (1- (* 8 shell)))) ; hit a corner?
		      (setf direction (third (gethash direction dir-incr))))
		  (incf x (first  (gethash direction dir-incr)))
		  (incf y (second (gethash direction dir-incr))))))))

    ;; Now walk the spiral and fill in the sums
    (format t "Spiral contains ~A cells.~%" (length *table*))
    (loop for i from 0 to (1- (length *table*))
       do (let ((tval (aref *table* i)))
	    (destructuring-bind (x y sum) tval
	      (incf sum (cell-value-at-coords (1+ x) y))
	      (incf sum (cell-value-at-coords (1+ x) (1+ y)))
	      (incf sum (cell-value-at-coords x      (1+ y)))
	      (incf sum (cell-value-at-coords (1- x) (1+ y)))
	      (incf sum (cell-value-at-coords (1- x) y))
	      (incf sum (cell-value-at-coords (1- x) (1- y)))
	      (incf sum (cell-value-at-coords x      (1- y)))
	      (incf sum (cell-value-at-coords (1+ x) (1- y)))
;	      (format t "~A: (~A,~A) = ~A~%" i x y sum)
	      (setf (aref *table* i) (list x y sum))
	      (if (> sum target-value)
		  (progn
		    (format t "Next largest sum is ~A at index ~A~%" sum i)
		    (sb-ext:exit :code 0))))))
  
    (format t "not found by index ~A.  Try a table with more shells?~%" (length *table*))
    (sb-ext:exit :code 1)))

(defun find-dist (number)
  (if (= 1 number) 0
      (let ((sum 1)
	    (n 1))
	; find the shell
	(do ()
	    ((<= number (+ sum (* n 8))))
	  (incf sum (* 8 n))
	  (incf n))
	; now, sum+1 is the shell-base and n is the shell number
	(let* ((shell-index (- number (1+ sum)))
	       (side-index (mod shell-index (* 2 n))))
	  (+ n (abs (- side-index (- n 1))))))))

(defun main (args)
  (let ((opts '(("s" :required 6)
		("v" :required 1))))
    (do ((targs (subseq args 1))
	 (done nil))
	((or (null targs) done))
      (multiple-value-bind (new-args val) (getopt:getopt targs opts)
	(if val
	    (cond ((string= "s" (caar val))
		   (setf *max-shells* (parse-integer (cdar val))))
		  ((string= "v" (caar val))
		   (setf *target-value* (parse-integer (cdar val)))))
	    (setf done t))
	(setf targs new-args))))
  
  (format t "Dist(~A) = ~A~%" *target-value* (find-dist *target-value*))
  (format t "Next Larger(~A) = ~A~%" *target-value* (next-larger *target-value*)))

(main sb-ext:*posix-argv*)
