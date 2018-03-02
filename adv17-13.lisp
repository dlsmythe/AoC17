;; sbcl --noinform --load adv17-13.lisp < adv17-13.input

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "cl-ppcre" :silent t)

(defparameter *verbose* nil)
(defparameter *max-depth* 0)

(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
       do (princ element out)
       when more
       do (princ separator out))))

(defun read-input ()
  (let ((i (make-hash-table)))
    (loop for line = (read-line *STANDARD-INPUT* nil)
       while line
       do (multiple-value-bind (matched l) (cl-ppcre:scan-to-strings "(\\d+):\\s*(\\d+)" (string-trim '(#\Newline) line))
	    ;;(format t "line ~A: p/w: ~A~%" line l)
	    (when (not matched)
	      (format t "bad line: ~a~%" line)
	      (sb-ext:exit :code 1))
	    (let* ((depth (parse-integer (elt l 0)))
		   (rng (parse-integer (elt l 1)))
		   (state (make-hash-table)))
	      (if (> depth *max-depth*)
		  (setf *max-depth* depth))
	      (setf (gethash 'range state) rng)
	      (setf (gethash 'scanpos state) 0)
	      (setf (gethash 'incr state) 1)
	      (setf (gethash depth i) state))))
    i))

(defun prt-state (states pos)
  (dotimes (i (1+ *max-depth*))
    (let ((state (gethash i states)))
      (if (null state)
	  (format t "~3d ~[...~;(.)~]~%" i (if (= i pos) 1 0))
	  (let ((scanpos (gethash 'scanpos state))
		(r (gethash 'range state)))
	    (format t "~3d ~a~%" i 
		    (with-output-to-string (layer)
		      (dotimes (j r)
			(let ((print-parens (if (and (= 0 j) (= i pos)) 0 1)))
			  (format layer "~[(~]~[_~;S~]~[)~]"
				  print-parens
				  (if (= j scanpos) 1 0)
				  print-parens)))))))))
  (format t "~%"))

(defun advance-scanners (state)
  (dotimes (i (1+ *max-depth*))
    (let ((s (gethash i state)))
      (when s
	(let ((scanpos (+ (gethash 'scanpos s) (gethash 'incr s))))
	  (cond ((>= scanpos (gethash 'range s))
		 (decf scanpos 2)
		 (let ((in (gethash 'incr s)))
		   (setf (gethash 'incr s) (- in))))
		((= -1 scanpos)
		 (setf scanpos 1)
		 (let ((in (gethash 'incr s)))
		   (setf (gethash 'incr s) (- in)))))
	  (setf (gethash 'scanpos s) scanpos))))))

(defun check-caught (pos state)
  (let ((s (gethash pos state)))
    (cond ((and s (= 0 (gethash 'scanpos s)))
	   (if *verbose*
	       (format t "caught at depth ~a range ~a~%" pos (gethash 'range s)))
	   (* pos (gethash 'range s)))
	  (t
	   0))))

(defun do-trip (s delay)
  (if *verbose*
      (if (> delay 0)
	  (format t "start delay ~a~%" delay)))
  (dotimes (i delay)
    (advance-scanners s))
  (if *verbose*
      (if (> delay 0)
	  (format t "end delay ~a~%" delay)))
  (let ((severity 0))
    (dotimes (i (1+ *max-depth*))
      (when *verbose*
	  (format t "Step ~a:~%" i)
	  (prt-state s i))
      (incf severity (check-caught i s))
      (advance-scanners s)
      (if *verbose*
	  (format t "trip severity: ~a~%" severity)))
    severity))

(defun copy-table (table)
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))

(defun main (args)
  (declare (ignore args))

  (format t "reading input~%")
  (let ((inp (read-input))
	(rlow 3930000)
	(rhigh 4000000))
    (format t "trying range(~a,~a)~%" rlow rhigh)
    (loop for i from rlow below rhigh
       do (let ((sev (do-trip (copy-table inp) i)))
	    (when (= 0 sev)
	      (format t "smallest delay: ~a~%" i)
	      (sb-ext:exit :code 0))
	    (if *verbose*
		(format t "delay ~a: severity ~a~%" i sev))
	    (if (= 0 (mod i 1000))
		(format t "."))
	    (finish-output)))
    (format t "try a bigger delay~%"))
  1)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
