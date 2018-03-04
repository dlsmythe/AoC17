;; sbcl --noinform --load adv17-13.lisp [-v n] [-l n] [-h n] < adv17-13.input
;;  -v n  set verbosity to level n
;;  -l n  set low end of ranges of delays to consider to n
;;  -h n  set high end of ranges of delays to consider to n
;;
;; Notes:
;;  This is a translation of the second attempt at solving this one, in C.
;;  There is still much room for improvement.
;;
;; New here:
;; - maphash
;; - mapcar
;; - find-if
;; - explicitly setting pointers to nil to avoid blowing the heap (see fw-state-free)
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

(defgeneric layer-init (l d r s i))
(defmethod layer-init ((l layer) d r s i)
  (with-slots (depth range scanpos increment) l
    (setf depth d)
    (setf range r)
    (setf scanpos s)
    (setf increment i))
  l)

;; This is the initial description of the firewall, read from the user. It is a layer list.
(defparameter *input-layers* nil)

;; This is a cache of class 'layer' objects, to avoid constant malloc/free
(defparameter *node-heap* (make-array '(10000) :initial-element nil))

;; This is an optimization for finding open slots in *node-heap*
;; It might not actually be free, you have to check.  But it's a
;; good place to start looking.
(defparameter *next-free-node* 0)

(defun layer-alloc (d r s increment)
  (let ((maxlen (length *node-heap*)))
    (do ((i 0 (1+ i))
	 (pos *next-free-node* (1+ pos)))
	((= maxlen i))
      (if (>= pos maxlen)
	  (setf pos 0))
      (let ((node (elt *node-heap* pos)))
	(unless node
	  (setf node (make-instance 'layer :index pos :depth d :range r :scanpos s :increment increment))
	  (setf (elt *node-heap* pos) node)
	  (setf *next-free-node* (1+ pos))
	  (return-from layer-alloc node))
	(when (= 0 (layer-range node))
	  (layer-init node d r s increment)
	  (setf *next-free-node* (1+ pos))
	  (return-from layer-alloc node)))))
  (format t "heap exhausted")
  (sb-ext:exit :code 1))

;; Update the scan position for this layer
(defgeneric layer-advance (layer))
(defmethod layer-advance ((l layer))
  (when l
    (with-slots (increment scanpos range) l
      (let ((newpos (+ scanpos increment)))
	(cond ((>= newpos range)
	       (decf newpos 2)
	       (setf increment (- increment)))
	      ((= -1 newpos)
	       (setf newpos 1)
	       (setf increment (- increment))))
	(setf scanpos newpos))))
  l)

;; returns a vector of layer nodes
(defun read-input ()
  (let* ((layer-list
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
	 (v (make-array (list (1+ *max-depth*)) :initial-element nil)))
    (dolist (l layer-list)
      (setf (elt v (layer-depth l)) l))
    v))

;;===================================================================

(defclass fw-state ()
  ((simtime :initarg :simtime :initform 0 :accessor fw-state-simtime)
   (layers :initarg :layers :initform nil :accessor fw-state-layers)))

(defmethod print-object ((s fw-state) stream)
  (with-slots (simtime layers) s
    (format stream "#<fw-state: simtime ~a" simtime)
    (loop for l across layers do (format stream "~%  ~a" l))
    (format stream ">~%")))

(defgeneric prt-state (fwstate pos))
(defmethod prt-state ((fwstate fw-state) pos)
  (with-slots (simtime layers) fwstate
    (format t "Time ~d step ~d:~%" simtime pos)
    (dotimes (i (1+ *max-depth*))
      (let ((n (elt layers i)))
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

(defun free-layers (l)
  (let ((set-last-free nil))
    (loop for i upto *max-depth*
       do (let ((n (elt l i)))
	    (when n
	      (unless set-last-free
		(setf set-last-free t)
		(setf *next-free-node* (layer-index n)))
	      (setf (layer-range n) 0))))))

(defun copy-layers (v-old)
  (let ((v-new (make-array (list (1+ *max-depth*)))))
    (map-into v-new (lambda (n)
		      (if n (layer-alloc (layer-depth n)
					 (layer-range n)
					 (layer-scanpos n)
					 (layer-increment n))
			  nil)) v-old)
    v-new))

(defgeneric fw-state-free (s))
(defmethod fw-state-free ((s fw-state))
  (with-slots (layers) s
    (free-layers layers)
    (setf layers nil))) ;; hopefully avoid some garbage

;; *simtimelist* is a queue containing elements of type fw-state
(defparameter *simtimelist* (make-instance 'dls:queue))

;; *simtimelist* keeps the last *max-depth* picoseconds of data so we don't have to keep regenerating it
;; This discards the oldest data.
(defun prune-simlist ()
  (do ()
      ((<= (dls:nmembers *simtimelist*) (1+ *max-depth*)))
    (let ((s (dls:dequeue *simtimelist*)))
      (if (> *verbose* 2)
	  (format t "Deleted state at time ~d~%" (fw-state-simtime s)))
      (fw-state-free s))))

;; The new state is either the original state at time 0, or just
;; the previous state, one tick later with the scanners advanced by one.
(defun fw-state-add (simtime prevstate)
  (let* ((s (make-instance 'fw-state :simtime simtime)))
    (if (= 0 simtime)
	(setf (fw-state-layers s) (copy-layers *input-layers*))
	(let ((layers (copy-layers (fw-state-layers prevstate))))
	  (map-into layers (lambda (l) (if l (layer-advance l))) layers)
	  (setf (fw-state-layers s) layers)))
    (dls:enqueue *simtimelist* s)
    (if (> *verbose* 2)
	(format t "Added state at time ~d~%" simtime))
    (prune-simlist)
    s))

;; Returns the fw-state at the given simulation time.
;; If it doesn't know the state already, it figures it out,
;; and caches the most recent *max-depth*+1 states.
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
	(setf s prev)
	(do ((i (1+ max-seen) (1+ i)))
	    ((> i time))
	  (when (and (> num-missing 1) (> i 0) (= (mod i 100000) 0))
	    (format t "~a " i)
	    (finish-output))
	  (setf s (fw-state-add i s)))
	(if (> num-missing 1)
	    (format t "~%Missing states added~%"))))
    s))

;; ====================

(defun check-caught (depth layers)
  (let ((n (elt layers depth)))
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
    (setf *input-layers* (read-input))
    (format t "~a layers, *max-depth* is ~a~%" (length *input-layers*) *max-depth*)

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
