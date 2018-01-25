;;; Run with: sbcl --noinform --load adv17-4.lisp
;;;  options:
;;;   -d <n>  enable debug level n

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *debug* nil)
(defparameter *input-file* "/mnt/hgfs/C/cygwin64/home/dlsmyth/adv17-4a.actual_input")

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

  ;; The first of the remaining args, if any, replaces the default input file name.
  (if args (setf *input-file* (car args)))

  ;; Count the number of lines with no duplicate words (or anagrams - pt2)
  (let ((valid-phrases-1 0)
	(valid-phrases-2 0))
    (with-open-file (in *input-file*)
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	;; Note: the copies are needed because the sort function is destructive
	(let* ((wv       (split-sequence:split-sequence #\space line))
	       (wvsorted (loop for s in (copy-tree wv) collect (sort (copy-seq s) #'char<)))
	       (wvset1   (remove-duplicates wv       :test #'string=))
	       (wvset2   (remove-duplicates wvsorted :test #'string=)))
	  
	  (if *debug*
	      (format t "Line: '~A'~% (~A) wv ~A~% (~A) wvs ~A~% (~A) wvset1 ~A~% (~A) wvset2 ~A~%"
		      line (length wv) wv (length wvsorted) wvsorted (length wvset1) wvset1 (length wvset2) wvset2))

	  (if (= (length wv) (length wvset1))
	      (incf valid-phrases-1))
	  (if (= (length wv) (length wvset2))
	      (incf valid-phrases-2)))))

    (format t "Part 1: ~A~%Part 2: ~A~%" valid-phrases-1 valid-phrases-2))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
