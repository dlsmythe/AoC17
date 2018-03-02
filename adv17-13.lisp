;; sbcl --noinform --load adv17-13.lisp [-v n] [-l n] [-h n] < adv17-13.input
;;  -v n  set verbosity to level n
;;  -l n  set low end of ranges of delays to consider to n
;;  -h n  set high end of ranges of delays to consider to n
;;
;; Notes:
;;  This is a translation of the second attempt at solving this one, in C.
;;  There is still much room for improvement.  For instance, instead of representing
;;  the state of the firewall as a list of layers, a vector would be much better.

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *verbose* 0)
(defparameter *max-depth* 0) ;; length of *node-heap* below should be this squared

;; First, a few generic helper routines

(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
       do (princ element out)
       when more
       do (princ separator out))))

(defun copy-table (table)
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))

;;===========

(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
	     ;; at least five elements, so print ellipsis
	     (format stream "(~{~S ~}... ~S)"
		     (subseq list 0 3) (first tail)))
	    ;; otherwise print whole list
	    (t (format stream "~:S" list))))))

(defgeneric dequeue (queue)
  (:documentation "Remove the next item from the front of the queue."))
(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defgeneric enqueue (queue item)
  (:documentation "Add an item to the end of the queue."))
(defmethod enqueue ((queue queue) new-item)
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
	    (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)

(defgeneric contains (queue item)
  (:documentation "Return whether an item is in the queue."))
(defmethod contains ((queue queue) item)
  (with-slots (list) queue
      (find item list)))

(defgeneric empty (queue)
  (:documentation "Return whether the queue is empty."))
(defmethod empty ((queue queue))
  (with-slots (list) queue
      (null list)))

(defgeneric members (queue))
(defmethod members ((queue queue))
  (slot-value queue 'list))

;;=========================================================

;; This defines the state of a particular layer within the firewall
(defclass layer ()
  ((index :initarg :index :initform 0 :accessor layer-index)
   (depth :initarg :depth :initform 0 :accessor layer-depth)
   (range :initarg :range :initform 0 :accessor layer-range)
   (scanpos :initarg :scanpos :initform 0 :accessor layer-scanpos)
   (increment :initarg :increment :initform 1 :accessor layer-increment)))

;; This is the initial description of the firewall, read from the user.
(defparameter *input-list* nil)

;; This is a cache of class 'layer' objects, to avoid constant malloc/free
(defparameter *node-heap* (make-array '(10000) :initial-element nil))

;; This is an optimization for finding open slots in *node-heap*
(defparameter *last-free* 0)

(defun layer-alloc (d r s increment)
  (let ((maxlen (length *node-heap*)))
    (do ((i 0 (1+ i))
	 (pos *last-free* (1+ pos)))
	((= maxlen i))
      (if (>= pos maxlen)
	  (setf pos 0))
      (let ((node (elt *node-heap* pos)))
	(unless node
	  (setf node (make-instance 'layer :index pos :depth d :range r :scanpos s :increment increment))
	  (setf (elt *node-heap* pos) node)
	  (setf *last-free* (1+ pos))
	  (return-from layer-alloc node))
	(when (= 0 (layer-range node))
	  (setf (layer-range node) r)
	  (setf (layer-depth node) d)
	  (setf (layer-increment node) increment)
	  (setf (layer-scanpos node) s)
	  (setf *last-free* (1+ pos))
	  (return-from layer-alloc node)))))
  (format t "heap exhausted")
  (sb-ext:exit :code 1))

(defun layer-free (node)
  (setf (layer-range node) 0))

(defun free-layer-list (l)
  (setf *last-free* (layer-index (car l)))
  (mapcar #'layer-free l))

(defun copy-layer-list (l)
  (loop for n in l collect (layer-alloc (layer-depth n) (layer-range n) (layer-scanpos n) (layer-increment n))))

(defun find-layer-with-depth (l depth)
  (find-if (lambda (n) (= depth (layer-depth n))) l))

;;===================================================================

;; Keep the last 100 picoseconds of data so we don't have to keep regenerating it
(defparameter *max-to-keep* 100)
(defparameter *max-kept* 0)

(defclass fw-state ()
  ((simtime :initarg :simtime :initform 0 :accessor fw-state-simtime)
   (layers :initarg :layers :initform nil :accessor fw-state-layers)))

;; *simtimelist* is a queue containing elements of type fw-state
(defparameter *simtimelist* (make-instance 'queue))

(defun prune-simlist ()
  (do ()
      ((<= *max-kept* *max-to-keep*))
    (let ((s (dequeue *simtimelist*)))
      (if (> *verbose* 1)
	  (format t "Deleted state at time ~d~%" (fw-state-simtime s)))
      (free-layer-list (fw-state-layers s))
      (setf (fw-state-layers s) nil) ;; hopefully avoid some garbage
      (decf *max-kept*))))

(defun add-fw-state (s)
  (if (> *verbose* 1)
      (format t "Added state at time ~d~%" (fw-state-simtime s)))
  (enqueue *simtimelist* s)
  (incf *max-kept*)
  (prune-simlist))

;; ====================

;; returns a list of layer nodes
(defun read-input ()
  (loop for line = (read-line *STANDARD-INPUT* nil)
     while line
     collect (multiple-value-bind (matched l)
		 (cl-ppcre:scan-to-strings
		  "(\\d+):\\s*(\\d+)" (string-trim '(#\Newline) line))
	       ;;(format t "line ~A: p/w: ~A~%" line l)
	       (when (not matched)
		 (format t "bad line: ~a~%" line)
		 (sb-ext:exit :code 1))
	       (let* ((depth (parse-integer (elt l 0)))
		      (rng   (parse-integer (elt l 1))))
		 (if (> depth *max-depth*)
		     (setf *max-depth* depth))
		 (layer-alloc depth rng 0 1)))))

(defun prt-state (states pos)
  (dotimes (i (1+ *max-depth*))
    (let ((n (find-layer-with-depth states i)))
      (if (null n)
	  (format t "~3d ~[...~;(.)~]~%" i (if (= i pos) 1 0))
	  (let ((scanpos (layer-scanpos n))
		(r (layer-range n)))
	    (format t "~3d ~a~%" i 
		    (with-output-to-string (layer)
		      (dotimes (j r)
			(let ((print-parens (if (and (= 0 j) (= i pos)) 0 1)))
			  (format layer "~[(~]~[_~;S~]~[)~]"
				  print-parens
				  (if (= j scanpos) 1 0)
				  print-parens)))))))))
  (format t "~%"))

;; Update the scan positions for each defined depth in this state
(defun advance-scanners (state)
  (dolist (s state)
    (let* ((increment (layer-increment s))
	   (scanpos (+ (layer-scanpos s) increment)))
      (cond ((>= scanpos (layer-range s))
	     (decf scanpos 2)
	     (setf (layer-increment s) (- increment)))
	    ((= -1 scanpos)
	     (setf scanpos 1)
	     (setf (layer-increment s) (- increment))))
      (setf (layer-scanpos s) scanpos)))
  state)

;; Returns a layer-list from a fw-state
(defun fw-state-at-time (time)
  ;; (format t "fw-state-at-time ~a~%" time)
  (let* ((max-seen -1)
	 (prev nil)
	 (s (find-if (lambda (n)
		       (let ((simtime (fw-state-simtime n)))
			 (when (> simtime max-seen)
			   (setf max-seen simtime)
			   (setf prev n))
			 (= simtime time))) (members *simtimelist*))))
    ;; (format t "initial search: ~a max-seen: ~a~%" s max-seen)
    (unless s
      (let ((num-missing (- time max-seen)))
	(if (> num-missing 1)
	    (format t "adding ~a missing states~%" num-missing))
	(do ((i (1+ max-seen) (1+ i)))
	    ((> i time))
	  (when (and (> num-missing 1) (= (mod i 100000) 0))
	    (format t "~a " i)
	    (finish-output))
	  ;; The new state is either the original state at time 0, or just
	  ;; the previous state, one tick later with the scanners advanced by one.
	  (setf s (make-instance
		   'fw-state
		   :simtime i
		   :layers (if (= 0 i)
			       (copy-layer-list *input-list*)
			       (let ((l (copy-layer-list (fw-state-layers prev))))
				 (advance-scanners l)))))
	  (add-fw-state s)
	  (setf prev s))
	(if (> num-missing 1)
	    (format t "Missing states added~%"))))
    (fw-state-layers s)))

;; state is a list of layers
;; returns (caught severity)
(defun check-caught (depth state)
  (let ((n (find-layer-with-depth state depth)))
    (cond ((and n (= 0 (layer-scanpos n)))
	   (if (> *verbose* 0)
	       (format t "caught at depth ~a range ~a~%" depth (layer-range n)))
	   (values 1 (* depth (layer-range n))))
	  (t
	   (values 0 0)))))

(defun do-trip (delay)
  (let ((severity 0))
    (loop for i from 0 to *max-depth*
       do (let ((state (fw-state-at-time (+ i delay))))
	    (when (> *verbose* 0)
	      (format t "Time ~d step ~d:~%" (+ i delay) i)
	      (prt-state state i))
	    (multiple-value-bind (caught sev) (check-caught i state)
	      (if caught
		  (incf severity sev)))))
    severity))

(defun main (args)
  (let ((rlow 0)
	(rhigh 1))

    ;; Parse command-line options
    (let ((opts '(("v" :required 0)
		  ("l" :required 0)
		  ("h" :required 1))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "l" (car arg))
		 (setf rlow (parse-integer (cdr arg))))
		((string= "h" (car arg))
		 (setf rhigh (parse-integer (cdr arg))))))
	(setf args new-args)))

    (format t "reading input~%")
    (setf *input-list* (read-input))
    (format t "~a layers, *max-depth* is ~a~%" (length *input-list*) *max-depth*)

    (format t "trying range(~a,~a)~%" rlow rhigh)
    (loop for delay from rlow below rhigh
       do (let ((sev (do-trip delay)))
	    (when (= 0 sev)
	      (format t "smallest delay: ~a~%" delay)
	      (sb-ext:exit :code 0))
	    (if (> *verbose* 0)
		(format t "delay ~a: severity ~a~%" delay sev))
	    (when (= 0 (mod delay 1000))
	      (format t ".")
	      (finish-output)))))
  
  (format t "try a bigger delay~%")
  1)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
