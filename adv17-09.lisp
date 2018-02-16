;;; Run with: sbcl --noinform --load adv17-09.lisp

(defparameter *verbose* nil)
(defparameter *score* 0)
(defparameter *buf* nil)

(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
       do (princ element out)
       when more
       do (princ separator out))))

(defun read-input (fname)
  (let ((input nil)
	(in-garbage nil)
	(char-count 0)
	(garbage-count 0))
    (with-open-file (in fname)
      (do* ((ch (read-char in nil 'the-end) (read-char in nil 'the-end)))
	   ((not (characterp ch)))
	(incf char-count)
	(unless (char= #\Newline ch)
	  (cond ((char= #\! ch)
		 (read-char in nil)
		 (incf char-count))
		(in-garbage
		 (if (char= #\> ch)
		     (setf in-garbage nil)
		     (incf garbage-count)))
		((char= #\< ch)
		 (setf in-garbage t))
		(t
		 (push ch input))))))
    (format t "garbage_count: ~A~%" garbage-count)
    (concatenate 'string (nreverse input))))

(let ((groupnum 0)
      (pos 0))
  (defun read-group (depth)
    (incf groupnum)
    (let ((this-group groupnum)
	  (subgroups 0))
      (when *verbose*
        (format t "read_group[~A](~A) buf[~A]='~A' score=~A~%" this-group depth pos (elt *buf* pos) *score*))
      (do ((done nil))
	  ((or done (>= pos (length *buf*))))
	(when *verbose*
	  (format t "  group[~A](~A) buf[~A]='~A' score=~A~%" this-group depth pos (elt *buf* pos) *score*))
	(let ((c (elt *buf* pos)))
	  (incf pos)
	  (cond ((char= #\{ c)
		 (read-group (1+ depth))
		 (incf subgroups))
		((char= #\, c))
		((char= #\Newline c))
		((char= #\} c)
		 (when *verbose*
		   (format t "group[~A] has ~A subgroups~%" this-group subgroups))
		 (incf *score* depth)
		 (setf done t))
		(t
		 (format t "unexpected character at pos ~A: '~A'~%" (1- pos) c)
		 (sb-ext:exit :code 1)))))
      (when *verbose*
	(format t "read_group[~A]() score = ~A~%" this-group *score*)))))

(defun main (args)
  (declare (ignore args))
  (setf *buf* (read-input "adv17-09.input"))
  (read-group 0)
  (format t "score is ~A~%" *score*)
  0)
    
(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
