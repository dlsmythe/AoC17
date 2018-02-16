;;; Run with: sbcl --noinform --load adv17-07.lisp
;;;  options:
;;;   -d <n>  enable debug level n
;;;   -p <1|2> run problem part 1 or 2

(proclaim '(optimize (speed 3) (safety 0)))

(ql:quickload "getopt" :silent t)
;;(ql:quickload "cl-ppcre" :verbose nil :prompt nil)
(ql:quickload "cl-ppcre" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *debug* nil)
(defparameter *part* 1)

;;===================================================================

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

(defgeneric enqueue (item queue)
  (:documentation "Add an item to the end of the queue."))

(defgeneric contains (item queue)
  (:documentation "Return whether an item is in the queue."))

(defgeneric empty (queue)
  (:documentation "Return whether the queue is empty."))
  
(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
	    (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)

(defmethod contains (item (queue queue))
  (with-slots (list) queue
      (find item list)))

(defmethod empty ((queue queue))
  (with-slots (list) queue
      (null list)))

;;===================================================================

(defparameter *proctab* (make-hash-table :test 'equal))
(defparameter *rootprocname* nil)

(defun parent (procname)
  (loop for pn being the hash-keys in *proctab* using (hash-value p)
     do (progn
	  ;;(format t "Looking in proc ~A through ~A for ~A~%" pn (gethash 'subprocs p) procname)
	  (when (find procname (gethash 'subprocs p) :test #'string=)
	    ;;(format t "Parent of ~A is ~A~%" procname (gethash 'name p))
	    (return-from parent (gethash 'name p)))))
  (setf *rootprocname* procname)
  (format t "Root proc is ~A~%" *rootprocname*)
  nil)

(defun read-procs ()
  (loop for line = (read-line *STANDARD-INPUT* nil)
     while line
     do (multiple-value-bind (dummy l) (cl-ppcre:scan-to-strings "(\\S+)\\s+[(](\\d+)[)]" line)
	  (declare (ignore dummy))
	  ;;(format t "line ~A: p/w: ~A~%" line l)
	  (when l
	    (let ((procname (elt l 0))
		  (weight (parse-integer (elt l 1)))
		  (sidx (search "->" line))
		  (subs nil))
	      (when sidx
		(let ((tsubs (split-sequence:split-sequence #\, (string-trim '(#\Space #\Linefeed) (subseq line (+ 2 sidx))))))
		  (setf subs (loop for s in tsubs collect (string-trim '(#\Space) s)))))
	      (let ((proc (make-hash-table)))
		(setf (gethash 'name proc) procname)
		(setf (gethash 'weight proc) weight)
		(setf (gethash 'subweight proc) 0)
		(setf (gethash 'subprocs proc) subs)
		;; (format t "Read proc ~A w: ~A sw: ~A sb: ~A~%"
		;; 	      (gethash 'name proc)
		;; 	      (gethash 'weight proc)
		;; 	      (gethash 'subweight proc) subs)
		(setf (gethash procname *proctab*) proc))))))
  ;; (format t "Setting parent pointers~%")
  (loop for pn being the hash-keys in *proctab* using (hash-value p)
     do (setf (gethash 'parentname p) (parent pn))))

(defun add-nodes-top-down ()
  (let ((outq nil)
	(open-set (make-instance 'queue))
	(closed-set nil))
    (push *rootprocname* outq)
    (enqueue *rootprocname* open-set)
    (setf (gethash 'level (gethash *rootprocname* *proctab*)) 0)
    (do ()
	((empty open-set))
      (let* ((parentname (dequeue open-set))
	     (parent (gethash parentname *proctab*)))
	(dolist (child (gethash 'subprocs parent))
	  (setf (gethash 'level (gethash child *proctab*)) (1+ (gethash 'level parent)))
	  (unless (find child closed-set)
	    (unless (contains child open-set)
	      ;; (format t "Adding child ~A~%" child)
	      (enqueue child open-set)
	      (push child outq))))
        (setf closed-set (remove-duplicates (push parentname closed-set)))))
    outq))

(defun calc-weights ()
  (let ((pq (add-nodes-top-down))
	(pn nil))
    (do ()
	((null pq))
      (setf pn (pop pq))
      ;; (format t "CALC-WEIGHTS(~A)~%" pn)
      (let* ((proc (gethash pn *proctab*))
	     (subprocs (gethash 'subprocs proc)))
	(dolist (subp subprocs)
	  (let ((subproc (gethash subp *proctab*)))
	    (incf (gethash 'subweight proc) (+ (gethash 'weight subproc) (gethash 'subweight subproc)))))))))

(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
       do (princ element out)
       when more
       do (princ separator out))))

(defun dump-procs-work (pn)
  (let* ((proc (gethash pn *proctab*))
	 (weight    (gethash 'weight proc))
	 (subweight (gethash 'subweight proc))
	 (subprocs  (gethash 'subprocs proc)))
    (format t "~A~A w: ~A sw: ~A total: ~A~%"
	    (join "" (loop for i below (gethash 'level proc) collect "  "))
	    pn weight subweight (+ weight subweight))
    (dolist (child subprocs)
      (dump-procs-work child))))

(defun dump-procs ()
    (format t "=====================================~%")
    (dump-procs-work *rootprocname*)
    (format t "=====================================~%"))

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("d" :required 0)
		("p" :required 1))))
    (do ((targs (subseq args 1))
	 (done nil))
	((or (null targs) done) (setf args targs))
      (multiple-value-bind (new-args val) (getopt:getopt targs opts)
	(if val
	    (cond ((string= "d" (caar val))
		   (setf *debug* (parse-integer (cdar val))))
		  ((string= "p" (caar val))
		   (setf *part* (parse-integer (cdar val)))))
	    (setf done t))
	(setf targs new-args))))

  
  (read-procs)
  (calc-weights)
  ;; (dump-procs)

  (format t "== finding bad tower  ===================================~%")
  (let ((prevmx 0)
	(prevmn 0)
	(basenodename *rootprocname*))
    (do ((bnode (gethash basenodename *proctab*) (gethash basenodename *proctab*)))
	((null basenodename))
      (setf basenodename nil)
      (let ((subprocs (gethash 'subprocs bnode)))
	(when subprocs
	  (let ((mn most-positive-fixnum)
		(mx 0)
		(mxidx -1)
		(mxp nil)
		(mnidx -1)
		(mnp nil)
		(allsame t)
		(prev nil))
	    (loop for i below (length subprocs)
	       do (let* ((pn (elt subprocs i))
			 (p (gethash pn *proctab*))
			 (weight (+ (gethash 'weight p) (gethash 'subweight p))))
		    (when (> weight mx)
		      (setf mx weight)
		      (setf mxidx i)
		      (setf mxp p))
		    (when (< weight mn)
		      (setf mn weight)
		      (setf mnidx i)
		      (setf mnp p))
		    (when (and prev (/= weight (+ (gethash 'weight prev) (gethash 'subweight prev))))
		      (setf allsame nil))
		    (setf prev p)))
	    (if (not allsame)
		(progn
		  (format t "proc ~A tower ~A (~A) weight is high:~%" (gethash 'name bnode) mxidx (elt subprocs mxidx))
		  (loop for i below (length subprocs)
		     do (let* ((procname (elt subprocs i))
			       (proc (gethash procname *proctab*))
			       (weight (+ (gethash 'weight proc) (gethash 'subweight proc))))
			  (format t "  ~A: ~A~%" procname weight)
			  (when (= i mxidx)
			    (setf basenodename procname)
			    (setf prevmx mx)
			    (setf prevmn mn)))))
		(let ((adjustment (- prevmx prevmn)))
		  (format t "~A must be reduced by ~A to ~A~%"
			  (gethash 'name bnode) adjustment
			  (- (gethash 'weight bnode) adjustment))))))))) 0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
