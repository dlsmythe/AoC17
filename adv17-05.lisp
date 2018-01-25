;;; Run with: sbcl --noinform --load adv17-05.lisp < adv17-05.input 
;;;  options:
;;;   -d <n>  enable debug level n

(ql:quickload "getopt" :silent t)

(defparameter *debug* nil)
(defparameter *part* 2)

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("d" :required 0))))
    (do ((targs (subseq args 1))
	 (done nil))
	((or (null targs) done) (setf args targs))
      (multiple-value-bind (new-args val) (getopt:getopt targs opts)
	(if val
	    (cond ((string= "d" (caar val))
		   (setf *debug* (parse-integer (cdar val)))))
	    (setf done t))
	(setf targs new-args))))

  (let ((jumpv (make-array '(0) :fill-pointer 0 :adjustable t)))
    (do ((line (read-line *standard-input* nil)
	       (read-line *standard-input* nil)))
	((null line))
      (vector-push-extend (parse-integer line) jumpv))

    (format t "~A~%" (do ((pc 0)
			  (count 0 (1+ count)))
			 ((or (< pc 0) (>= pc (length jumpv))) count)
		       (let ((offset (aref jumpv pc)))
			 (if (= 1 *part*)
			     (incf (aref jumpv pc))
			     (incf (aref jumpv pc) (if (< offset 3) 1 -1)))
			 (incf pc offset)))))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
