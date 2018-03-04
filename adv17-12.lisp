;;; sbcl --noinform --load adv17-12.lisp < adv17-12.input
;;;
;;; New here:
;;; - more complex package handling (still wrong...)
;;; - sets
;;; - bfs
;;; - subclassing
;;; - set-difference

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "cl-ppcre" :silent t)
(ql:quickload "split-sequence" :silent t)

(load "bfs.lisp")

(defun read-input (fname)
  (let ((progs nil))
    (with-open-file (in fname)
      (loop for line = (read-line in nil)
	 while line
	 do (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^(\\d+)\\s+<->\\s+(.+)" line)
	      (declare (ignore parsedok))
	      (let ((id (elt vals 0))
		    (neighbs (elt vals 1)))
		(let ((prog (make-hash-table)))
		  (setf (gethash 'id prog) (parse-integer id))
		  (setf (gethash 'neighbors prog)
			(loop for p in (split-sequence:split-sequence #\, (string-trim '(#\Space #\Linefeed) neighbs))
			   collect (parse-integer p)))
		  (push prog progs))))))
    (nreverse progs)))

;; ==========================================

(defclass prog-bfs (dls:bfs)
  ((nodes :initarg :nodes :initform nil)))

(defgeneric dls:children-of (prog-bfs id)
  (:documentation "Return the ids of the children of the given node"))
(defmethod dls:children-of ((bfs prog-bfs) id)
  (with-slots (nodes) bfs
    (gethash 'neighbors (gethash id nodes))))
    
;; ==========================================
  
(defun main (args)
  (declare (ignore args))

  (let ((proglist (read-input "adv17-12.input"))
	(progtab (make-hash-table)))

    (dolist (p proglist)
      (setf (gethash (gethash 'id p) progtab) p))

    (let ((sleft (loop for k being the hash-keys in progtab collect k)))
      (do ((numgroups 0 (1+ numgroups)))
	  ((= 0 (length sleft)) (format t "There are ~a groups~%" numgroups))
	(let ((srch (make-instance 'prog-bfs :nodes progtab))
	      (s1 (make-array '(0) :fill-pointer 0 :adjustable t))
	      (goal (car sleft)))
	  ;; (setf (bfs-verbose srch) t)
	  (dls:visit-all srch goal (lambda (n v) (vector-push-extend n v) nil) s1)
	  (format t "node ~a can reach ~a others~%" goal (1- (length s1)))
	  (setf sleft (set-difference sleft (concatenate 'list s1)))))))
  0)

(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
