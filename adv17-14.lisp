;;; sbcl --noinform --load adv17-14.lisp [-v n] [-k keystr]
;;;  -v n    set verbosity to level n
;;;  -k str  provide an alternate key-string
;;;
;;; New here:
;;; - base conversions, using write-to-string and parse-integer
;;; - two-dimensional array usage

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(load "knot.lisp")

(defparameter *verbose* 0)

(defclass block-sim ()
  ((bstrs :initarg :bstrs :initform (make-array '(128) :initial-element "") :accessor block-sim-bstrs)
   (regions :initarg :regions :initform (make-hash-table))
   (regiontab :initarg :regiontab :initform (make-array '(128 128) :initial-element 0))
   (last-region-num :initform 0)
   (current-region :initform nil))
  (:documentation "bstrs is a 128-row array of 128-character strings encoded as binary"))

(defun block-sim-new (keystr)
  "Make an instance of a block-sim, using the given key"
  (let ((bsim (make-instance 'block-sim))
	(total 0))
    (dotimes (i 128)
      (multiple-value-bind (h dh) (dls:knot-hash (format nil "~a-~a" keystr i))
	(declare (ignore dh))
	(let* ((numeric-h (parse-integer h :radix 16))
	       (b (write-to-string numeric-h :base 2)))
	  ;;(format t "~a-~a -> ~a -> ~a = ~a~%" keystr i h b (count #\1 b))
	  (incf total (count #\1 b))
	  (setf (elt (block-sim-bstrs bsim) i) (format nil "~128,'0b" numeric-h)))))
    (format t "there are ~a total 1 bits~%" total)
    bsim))
    
(defgeneric next-region (bsim)
  (:documentation "return a new (unused) region number"))
(defmethod next-region ((bsim block-sim))
  (with-slots (last-region-num) bsim
    (incf last-region-num)))

(defgeneric add-member (bsim row col)
  (:documentation "add a cell to the current region's member list"))
(defmethod add-member ((bsim block-sim) row col)
  (with-slots (current-region regions regiontab) bsim
    (let ((r (gethash current-region regions)))
      (unless r
	(setf r (make-hash-table))
	(setf (gethash 'members r) (make-array '(0) :fill-pointer 0 :adjustable t))
	(setf (gethash current-region regions) r))
      (vector-push-extend (list row col) (gethash 'members r))
      (setf (aref regiontab row col) current-region))))

(defgeneric move-members (bsim fromreg toreg)
  (:documentation "move the cells from one region's member-list to another's"))
(defmethod move-members ((bsim block-sim) fromreg toreg)
  (with-slots (regions regiontab) bsim
    ;;(format t "move-members(from ~a, to ~a)~%" fromreg toreg)
    (loop for (row col) across (gethash 'members (gethash fromreg regions))
       do (progn
	    (vector-push-extend (list row col) (gethash 'members (gethash toreg regions)))
	    (setf (aref regiontab row col) toreg)))
    (setf (gethash 'members (gethash fromreg regions)) nil)))

(defgeneric find-regions (bsim)
  (:documentation "walk the blocks, and assign them to regions"))
(defmethod find-regions ((bsim block-sim))
  (with-slots (regions regiontab current-region bstrs) bsim
    (dotimes (row 128)
      (setf current-region nil)
      (dotimes (col 128)
	;; Determine the correct current-region for this cell
	(let ((prev-region (if (= 0 row) 0 (aref regiontab (1- row) col))))
	  (cond ((char-equal #\0 (elt (elt bstrs row) col)) ; empty spot - set current-region to nil
		 (setf current-region nil))
		((null current-region)	; new non-empty spot - set current-region to prev or new
		 (if (= row 0)
		     (setf current-region (next-region bsim))
		     (if (> prev-region 0)
			 (setf current-region prev-region)
			 (setf current-region (next-region bsim)))))
		(t
		 ;; check if we are merging with an existing region
		 (when (and (> row 0) (> prev-region 0) (/= prev-region current-region))
		   (move-members bsim current-region prev-region)
		   (setf current-region prev-region)))))
	;; Add the cell to the current-region
	(if current-region (add-member bsim row col))))
    (format t "There are ~a regions~%"
	    (loop for k being the hash-keys in regions using (hash-value r)
	       count (> (length (gethash 'members r)) 0)))))

(defun main (args)
  (let ((inp "uugsqrei")) ;; my required puzzle input

    ;; Parse command-line options
    (let ((opts '(("v" :required 0)
		  ("k" :required nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "k" (car arg))
		 (setf inp (cdr arg)))))
	(setf args new-args)))

    (find-regions (block-sim-new inp)))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
