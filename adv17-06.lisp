;;; Run with: sbcl --noinform --load adv17-06.lisp
;;;
;;; New here:
;;; - join
;;; - with-output-to-string
;;; - generic functions
;;; - loop "on" vs loop "across"
;;; - loop over hash-keys

(defparameter *debug* 0)

;; This is the puzzle input. The answer to part 1 is 4074 and part 2 is 2793.
(defparameter *banks* '#( 11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11 ))

;; This is the example input.  The answer to part 1 should be 5 and part 2 is 4.
;;(defparameter *banks* '#( 0 2 7 0))

(defparameter *bankhist* (make-hash-table :test 'equal))

(defgeneric join (seq sep))
(defmethod join ((l list) separator)
  (with-output-to-string (out)
    (loop for (element . more) on l
	  do (princ element out)
	  when more
	  do (princ separator out))))
(defmethod join ((v vector) separator)
  (join (loop for i across v collect i) separator))

(defun bank-str ()
  (join *banks* "-"))

(defun check-hist (cycle-number)
  (let* ((h (bank-str))
	 (n (gethash h *bankhist*)))
    (when n
      (format t "cycle after ~A iterations~%" cycle-number)
      (format t "cycle length is ~A~%" (- cycle-number n))
      (sb-ext:exit :code 0))
    (setf (gethash h *bankhist*) cycle-number)))

(defun main ()
  (do ((cycle-number 1 (1+ cycle-number)))
      (nil)
    (when (> *debug* 0)
      (format t "*bankhist*:~%")
      (loop for k being the hash-keys in *bankhist* using (hash-value v)
	 do (format t "  ~A: ~A~%" k v))
      (format t "~A: ~A~%" cycle-number (bank-str)))
    (let ((bmax 0)
	  (bmaxidx 0))
      (loop for i upto (1- (length *banks*))
	 do (let ((n (elt *banks* i)))
	      (if (> n bmax)
		  (progn
		    (setf bmax n)
		    (setf bmaxidx i)))))
      (when (> *debug* 0)
	(format t "highest is banks[~A] = ~A~%" bmaxidx bmax))
      (setf (elt *banks* bmaxidx) 0)
      (do ()
	  ((= 0 bmax))
	(incf bmaxidx)
	(if (= bmaxidx (length *banks*))
	    (setf bmaxidx 0))
	(incf (elt *banks* bmaxidx))
	(decf bmax))
      (check-hist cycle-number))))

(main)
