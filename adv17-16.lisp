;;; sbcl --noinform --load adv17-16.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;  -i n  set iteration count (default 1)
;;;
;;; New here:
;;; - substring manipulation
;;; - md5

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(defparameter *verbose* 0)

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "md5" :silent t)

(load "files.lisp")
(load "knot.lisp")

(defclass simstep ()
  ((stype :initarg :stype :initform nil :accessor stype)
   (pos1 :initarg :pos1 :initform nil :accessor pos1)
   (pos2 :initarg :pos2 :initform nil :accessor pos2)))

(defparameter *step-list* (make-array '(0) :fill-pointer 0 :adjustable t))

(defun step-new (s p1 p2)
  (make-instance 'simstep :stype s :pos1 p1 :pos2 p2))
 
(defun add-step (s p1 p2)
  (vector-push-extend (step-new s p1 p2) *step-list*))

(defmethod print-object ((s simstep) stream)
  (with-slots (stype pos1 pos2) s
    (format t "#<simstep ~a ~a ~a>" stype pos1 pos2)))

(defun read-steps (fname)
  (let ((steps (dls:read-file-into-string fname)))
    (loop for str in (split-sequence:split-sequence #\, (string-trim '(#\Space #\Linefeed) steps))
       do (let ((s)
		(p1)
		(p2)
		(ch (char str 0)))
	    (cond ((char= #\x ch)
		   (setf s #\x)
		   (setf p1 (parse-integer (subseq str 1) :junk-allowed t))
		   (setf p2 (parse-integer (subseq str (1+ (position #\/ str))))))
		  ((char= #\s ch)
		   (setf s #\s)
		   (setf p1 (parse-integer (subseq str 1)))
		   (setf p2 nil))
		  ((char= #\p ch)
		   (setf s #\p)
		   (setf p1 (char str 1))
		   (setf p2 (char str 3)))
		  (t
		   (format t "bogus step~%")
		   (sb-ext:exit :code 1)))
	    (add-step s p1 p2)))))

(defparameter *history* (make-array '(0) :fill-pointer 0 :adjustable t))

(defun addposition (s idx)
  ;; (format t "iter ~a ~a~%" idx s)
  (loop for i below (length *history*)
     do (when (string= s (elt *history* i))
	  (format t "same state at steps ~a and ~a: ~a~%" i idx (elt *history* i))
	  (let* ((cycle-count (- idx i))
		 (residual (mod 1000000000 cycle-count))
		 (endval (elt *history* residual)))
	    (format t "result at 1,000,000,000 is same as at ~a: ~a~%" residual endval))
	  (sb-ext:exit :code 0)))
  (vector-push-extend (copy-seq s) *history*))


(defun main (args)
  (let ((itercount 1))

    ;; Parse command-line options
    (let ((opts '(("v" :required 0)
		  ("i" :required 0))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "i" (car arg))
		 (setf itercount (parse-integer (cdr arg))))))
	(setf args new-args)))

    (read-steps "adv17-16.input")
    ;; (loop for s across *step-list* do (print s))
    (let ((numsteps (length *step-list*))
	  (positions "abcdefghijklmnop"))
      (format t "There are ~a steps~%" numsteps)
      (addposition positions 0)
      (loop for idx from 1 to itercount
	 do (progn
	      (loop for i below numsteps
		 do (let* ((s (elt *step-list* i))
			   (stype (stype s))
			   (pos1 (pos1 s))
			   (pos2 (pos2 s)))
		      (cond ((char= stype #\s) ; move pos1 chars from end to front
			     (when (> *verbose* 0)
			       (format t "spin ~a ~a~%" pos1 positions))
			     (setf positions (concatenate 'string
							  (subseq positions (- 16 pos1))
							  (subseq positions 0 (- 16 pos1))))
			     (when (> *verbose* 0)
			       (format t "result: ~a~%" positions)))
			    ((char= stype #\x)
			     (when (> *verbose* 0)
			       (format t "exchange ~a ~a ~a~%" pos1 pos2 positions))
			     (let ((c (char positions pos1)))
			       (setf (char positions pos1) (char positions pos2))
			       (setf (char positions pos2) c))
			     (when (> *verbose* 0)
			       (format t "result: ~a~%" positions)))
			    ((char= stype #\p)
			     (when (> *verbose* 0)
			       (format t "step p ~a ~a positions ~a~%" pos1 pos2 positions)
			       (format t "pos[1] ~a~%" (position pos1 positions))
			       (format t "pos[2] ~a~%" (position pos2 positions)))
			     (let* ((p1 (position pos1 positions))
				    (p2 (position pos2 positions))
				    (c (char positions p1)))
			       (setf (char positions p1) (char positions p2))
			       (setf (char positions p2) c))))))
	      (addposition positions idx)))
      (format t "final positions: ~a~%" positions)))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
