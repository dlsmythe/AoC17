;;; Run with: sbcl --noinform --load adv17-08.lisp < adv17-08.input

(ql:quickload "cl-ppcre" :silent t)

(defparameter *regvals* (make-hash-table :test 'equal))
(defparameter *regmaxval* 0)

;; returns a list of hashes, each element containing one insn
(defun read-insns ()
  (let ((prog nil)
        (linecount 0))
    (loop for line = (read-line *STANDARD-INPUT* nil)
       while line
       do (multiple-value-bind (matched l) (cl-ppcre:scan-to-strings "^(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+if\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)" line)
	    (incf linecount)
	    (unless matched
	      (format t "garbled line at ~A: ~A~%" linecount line)
	      (sb-ext:exit :code 1))
	    (let ((insn (make-hash-table)))
	      (setf (gethash 'line insn) linecount)
	      (setf (gethash 'destreg insn) (elt l 0))
	      (setf (gethash 'destop insn) (elt l 1))
	      (setf (gethash 'destarg insn) (parse-integer (elt l 2)))
	      (setf (gethash 'predreg insn) (elt l 3))
	      (setf (gethash 'predop insn) (elt l 4))
	      (setf (gethash 'predarg insn) (parse-integer (elt l 5)))
	      ;; (format t "line ~A: insn: [~A ~A ~A ~A ~A ~A ~A]~%" line
	      ;; 	      (gethash 'line insn)
	      ;; 	      (gethash 'destreg insn)
	      ;; 	      (gethash 'destop insn)
	      ;; 	      (gethash 'destarg insn)
	      ;; 	      (gethash 'predreg insn)
	      ;; 	      (gethash 'predop insn)
	      ;; 	      (gethash 'predarg insn))
	      (push insn prog))))
    (nreverse prog)))

(defun regval (regname)
  (multiple-value-bind (rval exists) (gethash regname *regvals*)
    (if exists rval 0)))

(defun update-reg (lineno regname op arg)
  (let* ((val (regval regname))
	 (newval (cond ((string= op "inc") (+ val arg))
		       ((string= op "dec") (- val arg))
		       (t
			(format t "Error at line ~A: unknown operation \"~A\"~%" lineno op)
			(sb-ext:exit :code 1)))))
    ;; (format t "reg[~A] = ~A, ~A ~A = ~A~%" regname val (if (string= op "inc") "+" "-") arg newval)
    (setf (gethash regname *regvals*) newval)
    (if (> newval *regmaxval*)
        (setf *regmaxval* newval))))

(defun pred-val (lineno regname op argval)
  (let ((rval (regval regname)))
    (let ((retval (cond ((string= op "==") (=  rval argval))
			((string= op "!=") (/= rval argval))
			((string= op "<")  (<  rval argval))
			((string= op "<=") (<= rval argval))
			((string= op ">")  (>  rval argval))
			((string= op ">=") (>= rval argval))
			(t
			 (format t "Error at line ~A: unknown relational operator \"~A\"~%" lineno op)
			 (sb-ext:exit :code 1)))))
      ;; (format t "reg[~A] (~A) ~A ~A ? ~A~%" regname rval op argval (if retval "YES" "NO"))
      retval)))

(defun main (args)
  (declare (ignore args))
  (let ((prog (read-insns)))
    (dolist (insn prog)
      (let ((line    (gethash 'line insn))
	    (predreg (gethash 'predreg insn))
	    (predop  (gethash 'predop insn))
	    (predarg (gethash 'predarg insn))
	    (destreg (gethash 'destreg insn))
	    (destop  (gethash 'destop insn))
	    (destarg (gethash 'destarg insn)))
	(when (pred-val line predreg predop predarg)
	  (update-reg line destreg destop destarg)))))

  (let ((rvs (loop for k being the hash-keys in *regvals*
		using (hash-value v) collect v))
	(regs (let ((keys (sort (loop for k being the hash-keys in *regvals* collect k) #'string<)))
		(loop for k in keys collect (gethash k *regvals*)))))
    (format t "largest val: ~A~%" (reduce #'max rvs))
    (format t "largest ever: ~A~%" *regmaxval*)
    (format t "Regs at end: ~A~%" regs))

  0)
 
(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
