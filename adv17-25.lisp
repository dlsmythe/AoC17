;;; Run with: sbcl --noinform --load adv17-25.lisp
;;;  options:
;;;   -d <debuglevel>
;;;   -S                       Read sample blueprint
;;;   -p <blueprintfile>       Read state-machine blueprint (default: adv17-25.input)

;;(proclaim '(optimize (debug 3)))
(proclaim '(optimize (speed 3) (safety 0)))

(ql:quickload "getopt" :silent t)

(defparameter *debug* 0)
(defparameter *infname* "adv17-25.input")

(defparameter *sample-blueprint*
  '("Begin in state A."
    "Perform a diagnostic checksum after 6 steps."
    ""
    "In state A:"
    "  If the current value is 0:"
    "    - Write the value 1."
    "    - Move one slot to the right."
    "    - Continue with state B."
    "  If the current value is 1:"
    "    - Write the value 0."
    "    - Move one slot to the left."
    "    - Continue with state B."
    ""
    "In state B:"
    "  If the current value is 0:"
    "    - Write the value 1."
    "    - Move one slot to the left."
    "    - Continue with state A."
    "  If the current value is 1:"
    "    - Write the value 1."
    "    - Move one slot to the right."
    "    - Continue with state A."))

(defstruct state-desc-inner (write-val 0) (move-incr 1) (next-state 0))
(defstruct state-desc (val nil))
(defparameter *states* (make-array '(26) :initial-element nil))

(defun check-state (state)
  (if (null (aref *states* state))
	(setf (aref *states* state)
	      (make-state-desc :val (list (make-state-desc-inner) (make-state-desc-inner))))))

(defun state-write-val (state n)
  (state-desc-inner-write-val (elt (state-desc-val (aref *states* state)) n)))
;(declaim (inline state-write-val))

(defun state-move-incr (state n)
  (state-desc-inner-move-incr (elt (state-desc-val (aref *states* state)) n)))
;(declaim (inline state-move-incr))

(defun state-next-state (state n)
  (state-desc-inner-next-state (elt (state-desc-val (aref *states* state)) n)))
;(declaim (inline state-next-state))

(defun set-write-val (state value-index value)
  (check-state state)
  (let* ((s (aref *states* state))
	 (val (elt (state-desc-val s) value-index)))
    (setf (state-desc-inner-write-val val) value)))
;(declaim (inline set-write-val))

(defun set-move-incr (state value-index value)
  (check-state state)
  (let* ((s (aref *states* state))
	 (val (elt (state-desc-val s) value-index)))
    (setf (state-desc-inner-move-incr val) value)))
;(declaim (inline set-move-incr))

(defun set-next-state (state value-index value)
  (check-state state)
  (let* ((s (aref *states* state))
	 (val (elt (state-desc-val s) value-index)))
    (setf (state-desc-inner-next-state val) value)))
;(declaim (inline set-next-state))

(defparameter *start-state* -1)
(defparameter *max-steps* 0)
(defparameter *cur-state* -1)
(defparameter *cur-value* -1)
(defparameter *cur-line* -1)

(defun str-starts-with-p (str1 str2)
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun process-blueprint-line (line)
;;  (format t "processing blueprint line ~A~%" line)
  (assert (not (null line)) (line))
  (setf line (string-left-trim '(#\Space) line))
  (incf *cur-line*)
  (unless (= 0 (length line))
    (cond ((str-starts-with-p line "Begin in state ")
	   (progn
	     (setf *start-state* (- (char-code (aref line 15)) (char-code #\A)))
	     (assert (>= *start-state* 0) (*start-state*))
	     (assert (< *start-state* 26) (*start-state*))))
	  ((str-starts-with-p line "Perform a diagnostic checksum after ")
	   (progn
	     (setf *max-steps* (parse-integer (subseq line 36) :junk-allowed t))
	     (assert (> *max-steps* 0) (*max-steps*))))
	  ((str-starts-with-p line "In state ")
	   (progn
	     (setf *cur-state* (- (char-code (aref line 9)) (char-code #\A)))
	     (assert (>= *cur-state* 0) (*cur-state*))
	     (assert (< *cur-state* 26) (*cur-state*))))
	  ((str-starts-with-p line "If the current value is ")
	   (let ((val (- (char-code (aref line 24)) (char-code #\0))))
	     (assert (or (= val 0) (= val 1)) (val))
	     (setf *cur-value* val)))
	  ((str-starts-with-p line "- Write the value ")
	   (let ((val (- (char-code (aref line 18)) (char-code #\0))))
	     (assert (or (= val 0) (= val 1)) (val))
	     (assert (>= *cur-state* 0) (*cur-state*))
	     (assert (< *cur-state* 26) (*cur-state*))
	     (assert (or (= *cur-value* 0) (= *cur-value* 1)) (*cur-value*))
	     (set-write-val *cur-state* *cur-value* val)))
	  ((str-starts-with-p line "- Move one slot to the ")
	   (let ((dir (aref (string-downcase line) 23)))
	     (assert (or (char= dir #\r) (char= dir #\l)) (dir))
	     (assert (>= *cur-state* 0) (*cur-state*))
	     (assert (< *cur-state* 26) (*cur-state*))
	     (assert (or (= *cur-value* 0) (= *cur-value* 1)) (*cur-value*))
	     (set-move-incr *cur-state* *cur-value* (if (char= dir #\r) 1 -1))))
	  ((str-starts-with-p line "- Continue with state ")
	   (let ((next-state (- (char-code (aref line 22)) (char-code #\A))))
	     (assert (>= next-state 0) (next-state))
	     (assert (< next-state 26) (next-state))
	     (assert (>= *cur-state* 0) (*cur-state*))
	     (assert (< *cur-state* 26) (*cur-state*))
	     (assert (or (= *cur-value* 0) (= *cur-value* 1)) (*cur-value*))
	     (set-next-state *cur-state* *cur-value* next-state)))
	  (t (progn (format t "~A: bogus input line: ~A~%" *cur-line* line)
		    (sb-ext:exit :code 1))))))

(defun read-sample-blueprint ()
  (format t "Reading sample blueprint~%")
  (dolist (line *sample-blueprint*)
    (process-blueprint-line line)))

(defun read-blueprint (fname)
  (format t "Reading blueprint from ~A~%" fname)
  (with-open-file (fp fname)
    (do ((line (read-line fp nil)
	       (read-line fp nil)))
	((null line))
      (process-blueprint-line (string-trim '(#\Space #\Newline) line)))))

(defun print-blueprint ()
  (format t "Begin in state ~C.~%" (code-char (+ *start-state* (char-code #\A))))
  (format t "Perform a diagnostic checksum after ~A steps.~%" *max-steps*)
  (do ((s 0 (1+ s)))
      ((>= s 26))
    (unless (null (aref *states* s))
      (format t "~%In state ~C:~%" (code-char (+ s (char-code #\A))))
      (format t "  If the current value is 0:~%")
      (format t "    - Write the value ~A.~%" (state-write-val s 0))
      (format t "    - Move one slot to the ~A.~%" (if (= 1 (state-move-incr s 0)) "right" "left"))
      (format t "    - Continue with state ~C.~%" (code-char (+ (state-next-state s 0) (char-code #\A))))
      (format t "  If the current value is 1:~%")
      (format t "    - Write the value ~A.~%" (state-write-val s 1))
      (format t "    - Move one slot to the ~A.~%" (if (= 1 (state-move-incr s 1)) "right" "left"))
      (format t "    - Continue with state ~C.~%" (code-char (+ (state-next-state s 1) (char-code #\A)))))))

(defparameter *cells* nil)
(defstruct cell pos (val 0))

(defun cell-at (pos)
  (let ((c (find pos *cells* :key (lambda (c) (cell-pos c)))))
    (if c
	c
	(let ((newc (make-cell :pos pos :val 0)))
	  (setf *cells* (merge 'list *cells* 
			       (list newc)
			       (lambda (x y) (< (cell-pos x) (cell-pos y)))))
	  (if (> *debug* 2)
	      (format t "added new cell at pos ~A~%" pos))
	  newc))))

(defun cell-val-at (pos)
  (cell-val (cell-at pos)))

(defun write-cell (pos val)
  (if (> *debug* 2)
      (progn
	(format t "write cell ~A = ~A~%" pos val)
	(format t "orig *cells*: ~A~%" *cells*)))
  (let ((c (cell-at pos)))
    (setf (cell-val c) val)
    (if (> *debug* 2)
	(format t "new *cells*: ~A~%" *cells*))
    c))

(defun diagnostic-checksum ()
  (reduce (lambda (x y) (+ (if (numberp x) x (cell-val x)) (cell-val y))) *cells*))
; (count-if (lambda (c) (= 1 (cell-val c))) *cells*))
  
(defparameter *CELLS-PER-LINE* 40)

(defmacro print-prefix (pos)
  `(if (/= 0 *CELLS-PER-LINE*)
      (if (= colcnt *CELLS-PER-LINE*)
	  (format t "  ~A: " ,pos))))

(defmacro print-suffix ()
  `(if (/= 0 *CELLS-PER-LINE*)
	    (if (= colcnt *CELLS-PER-LINE*)
		(progn
		  (format t "~%")
		  (setf colcnt 0)))))

(defmacro print-cell (pos val print-brackets)
  `(progn
     (print-prefix ,pos)
     (format t "~C~C~C"
	     (if ,print-brackets #\[ #\Space)
	     (code-char (+ ,val (char-code #\0)))
	     (if ,print-brackets #\] #\Space))
     (incf colcnt)
     (print-suffix)))

(defun print-tape (curpos)
  (when *cells*
    (let ((colcnt 0)
	  (pos (min curpos (cell-pos (first *cells*)))))
      (do ((clist *cells* (cdr clist)))
	  ((null clist))
	(let ((c (first clist)))
	  (print-cell pos (cell-val c) (= pos curpos))
	  (incf pos))

	;; print cells before the next defined one
	(when (cdr clist)
	  (let ((next-defined-pos (cell-pos (cadr clist))))
	    (do ()
		((= pos next-defined-pos))
	      ;(format t "inserting non-existent cell at pos ~A~%" pos)
	      (print-cell pos 0 (= pos curpos))
	      (incf pos)))))

      (if (> colcnt 0)
	  (format t "~%")))))

(defun main (args)
  ;;(trace main :report trace)
  (let ((print-bp nil))
    (let ((opts '(("d" :required 0)
		  ("S" :none)
		  ("p" :none)
		  )))
      (multiple-value-bind (new-args targs) (getopt:getopt args opts)
	(dolist (val targs)
	  (cond ((string= "d" (car val))
		 (setf *debug* (parse-integer (cdr val))))
		((string= "S" (car val))
		 (setf *infname* nil)
		 (read-sample-blueprint))
		((string= "p" (car val))
		 (setf print-bp t))))
	(setf args new-args))

      (if (cdr args)
	  (progn
	    (setf *infname* (last args))
	    (setf args (butlast args)))))

    (if *infname*
	(read-blueprint *infname*))
    
    (if print-bp
	(print-blueprint)))

  (format t "*debug* level is ~A~%" *debug*)
  (write-cell 0 0)
  (let ((pos 0)
	(cur-state *start-state*))
    (do ((step-count 0 (1+ step-count)))
	((>= step-count *max-steps*))

      (if (or (> *debug* 0) (and (> step-count 0) (= 0 (mod step-count 100000))))
	  (format t "TIME: ~A about to run state ~A, pos is ~A:~%" step-count (code-char (+ cur-state (char-code #\A))) pos))
      (if (null (aref *states* cur-state))
	  (progn
	    (format t "bogus state: ~C~%" (code-char (+ cur-state (char-code #\A))))
	    (sb-ext:exit :code 1)))

      (let* ((cell (cell-at pos))
	     (cur-val (cell-val cell))
	     (move-incr  (state-move-incr cur-state cur-val))
	     (write-val  (state-write-val cur-state cur-val))
	     (next-state (state-next-state cur-state cur-val)))
	(if (> *debug* 1)
	    (print-tape pos))
	(write-cell pos write-val)
	(incf pos move-incr)
	(setf cur-state next-state)))

    (if (> *debug* 0)
	(progn
	  (format t "final tape state ~A pos ~A:~%" (code-char (+ cur-state (char-code #\A))) pos)
	  (print-tape pos))))

  (format t "diagnostic checksum: ~A~%" (diagnostic-checksum))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
