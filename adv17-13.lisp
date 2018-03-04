;; sbcl --noinform --load adv17-13.lisp [-v n] [-l n] [-h n] < adv17-13.input
;;  -v n  set verbosity to level n
;;  -l n  set low end of ranges of delays to consider to n
;;  -h n  set high end of ranges of delays to consider to n
;;
;; Notes:
;;  This is a translation of the second attempt at solving this one, in C.
;;  There is still much room for improvement.  For instance, instead of representing
;;  the state of the firewall as a list of layers, a vector would be much better.
;;
;; New here:
;; - maphash
;; - mapcar
;; - find-if
;; - explicitly setting pointers to nil to avoid blowing the heap (see prune-simlist)
;; - assert

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(load "queues.lisp")

(defparameter *verbose* 0)
(defparameter *max-depth* 0) ;; length of *node-heap* below should be this squared

;;=========================================================

;; This defines the state of a particular layer within the firewall
(defclass layer ()
  ((index :initarg :index :initform 0 :accessor layer-index)
   (depth :initarg :depth :initform 0 :accessor layer-depth)
   (range :initarg :range :initform 0 :accessor layer-range)
   (scanpos :initarg :scanpos :initform 0 :accessor layer-scanpos)
   (increment :initarg :increment :initform 1 :accessor layer-increment)))

(defmethod print-object ((layer layer) stream)
  (with-slots (index depth range scanpos increment) layer
    (format stream "#<layer: index ~a depth ~a range ~a scanpos ~a increment ~a>" index depth range scanpos increment)))

;; This is the initial description of the firewall, read from the user. It is a layer list.
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
  (assert (not (null l)) (l))
  (assert (typep l 'cons) (l))
  (assert (typep (car l) 'layer) (l))
  (loop for n in l collect (layer-alloc (layer-depth n) (layer-range n) (layer-scanpos n) (layer-increment n))))

(defun find-layer-with-depth (l depth)
  (find-if (lambda (n) (= depth (layer-depth n))) l))

;; Update the scan position for this layer
(defgeneric layer-advance (layer))
(defmethod layer-advance ((l layer))
  (with-slots (increment scanpos range) l
    (let ((newpos (+ scanpos increment)))
      (cond ((>= newpos range)
	     (decf newpos 2)
	     (setf increment (- increment)))
	    ((= -1 newpos)
	     (setf newpos 1)
	     (setf increment (- increment))))
      (setf scanpos newpos)))
 l)

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

;;===================================================================

(defclass fw-state ()
  ((simtime :initarg :simtime :initform 0 :accessor fw-state-simtime)
   (layers :initarg :layers :initform nil :accessor fw-state-layers)))

(defmethod print-object ((s fw-state) stream)
  (with-slots (simtime layers) s
    (format stream "#<fw-state: simtime ~a" simtime)
    (dolist (l layers)
      (format stream "~%  ~a" l))
    (format stream ">~%")))

(defgeneric prt-state (fwstate pos))
(defmethod prt-state ((fwstate fw-state) pos)
  (with-slots (simtime layers) fwstate
    (format t "Time ~d step ~d:~%" simtime pos)
    (dotimes (i (1+ *max-depth*))
      (let ((n (find-layer-with-depth layers i)))
	(if (null n)
	    (format t "~3d ~[...~;(X)~]~%" i (if (= i pos) 1 0))
	    (let ((scanpos (layer-scanpos n))
		  (r (layer-range n)))
	      (format t "~3d ~a~%" i
		      (with-output-to-string (layer)
			(dotimes (j r)
			  (format layer "~a"
				  (cond ((= j 0)
					 (if (= 0 scanpos)
					     (if (= i pos) "*" "S")
					     (if (= i pos) "X" "_")))
					(t
					 (if (= j scanpos) "S" "_"))))))))))))
  (format t "~%"))

;; *simtimelist* is a queue containing elements of type fw-state
(defparameter *simtimelist* (make-instance 'dls:queue))

;; *simtimelist* keeps the last 100 picoseconds of data so we don't have to keep regenerating it
;; NB: this number was derived from the fact that the default input has ~100 layers.
(defconstant +max-to-keep+ 100)

(let ((max-kept 0))

  ;; This discards the oldest data, in accordance with +max-to-keep+.
  (defun prune-simlist ()
    (do ()
	((<= max-kept +max-to-keep+))
      (let ((s (dls:dequeue *simtimelist*)))
	(if (> *verbose* 2)
	    (format t "Deleted state at time ~d~%" (fw-state-simtime s)))
	(free-layer-list (fw-state-layers s))
	(setf (fw-state-layers s) nil) ;; hopefully avoid some garbage
	(decf max-kept))))

  ;; The new state is either the original state at time 0, or just
  ;; the previous state, one tick later with the scanners advanced by one.
  (defun fw-state-add (simtime prevstate)
    (when (> simtime 0)
      (assert (typep prevstate 'fw-state) (prevstate))
      (assert (not (null (fw-state-layers prevstate))) (prevstate)))
    (let* ((layerlist (if (= 0 simtime)
			  (copy-layer-list *input-list*)
			  (mapcar #'layer-advance (copy-layer-list (fw-state-layers prevstate)))))
	   (s (make-instance 'fw-state :simtime simtime :layers layerlist)))
      (if (> *verbose* 2)
	  (format t "Added state at time ~d~%" simtime))
      (dls:enqueue *simtimelist* s)
      (incf max-kept)
      (prune-simlist)
      (assert (typep s 'fw-state) (s))
      (assert (not (null (fw-state-layers s))) (s))
      (assert (typep (first (fw-state-layers s)) 'layer) (s))
      s)))

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
			 (= simtime time)))
		     (dls:members *simtimelist*))))
    ;; (format t "initial search: ~a max-seen: ~a~%" s max-seen)
    (unless s
      (let ((num-missing (- time max-seen)))
	(if (> num-missing 1)
	    (format t "adding ~a missing states~%" num-missing))
	(do ((i (1+ max-seen) (1+ i)))
	    ((> i time))
	  (when (and (> num-missing 1) (> i 0) (= (mod i 100000) 0))
	    (format t "~a " i)
	    (finish-output))
	  (setf s (fw-state-add i prev))
	  (setf prev s))
	(if (> num-missing 1)
	    (format t "Missing states added~%"))))
    s))

;; ====================

(defun check-caught (depth layerlist)
  (let ((n (find-layer-with-depth layerlist depth)))
    (values (and n (= 0 (layer-scanpos n))) (if n (* depth (layer-range n)) 0))))

(defun do-trip (delay)
  (let ((severity 0)
	(ever-caught nil))
    (loop for i from 0 to *max-depth*
       do (let ((state (fw-state-at-time (+ i delay))))
	    (when (> *verbose* 0)
	      (prt-state state i))
	    (multiple-value-bind (caught sev) (check-caught i (fw-state-layers state))
	      (when caught
		(if (> *verbose* 0)
		    (format t "caught at depth ~a severity ~a~%" i sev))
		(setf ever-caught t)
		(incf severity sev)))))
    (values ever-caught severity)))

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
       do (multiple-value-bind (caught sev) (do-trip delay)
	    (when (and (not caught) (= 0 sev))
	      (format t "smallest delay: ~a~%" delay)
	      (sb-ext:exit :code 0))
	    (if (or (> *verbose* 0) (= (1- rhigh) delay))
		(format t "delay ~a: severity ~a~%" delay sev))
	    (when (= 0 (mod delay 10000))
	      (format t ".")
	      (finish-output)))))
  
  (format t "try a bigger delay~%")
  1)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
