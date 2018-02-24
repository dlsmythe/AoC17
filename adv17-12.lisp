;; sbcl --noinform --load adv17-12.lisp < adv17-12.input

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "cl-ppcre" :silent t)
(ql:quickload "split-sequence" :silent t)

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

;;===================================================================

(defclass item-set ()
  ((memberlist :initarg :initlist :initform nil :accessor members)))

(defgeneric set-contains (item-set item)
  (:documentation "return when item is a member of the set"))
(defmethod set-contains ((item-set item-set) item)
  (with-slots (memberlist) item-set
  (member item memberlist)))

(defgeneric set-add (item-set item)
  (:documentation "Add given item to the set."))
(defmethod set-add ((item-set item-set) item)
  (with-slots (memberlist) item-set
    (setf memberlist (adjoin item memberlist))))

(defgeneric set-union (item-set item-set)
  (:documentation "Add the unique elements of setb to seta."))
(defmethod set-union ((seta item-set) setb)
  (with-slots (memberlist) seta
    (setf memberlist (union memberlist (members setb)))))

(defgeneric set-subtract (seta setb)
  (:documentation "Remove elements from seta that are in setb"))
(defmethod set-subtract ((seta item-set) setb)
  (with-slots (memberlist) seta
    (setf memberlist (mapcar (lambda (x) (remove x memberlist)) (members setb)))))

(defgeneric set-pop (item-set)
  (:documentation "Returns a member of the set, or nil if it is empty."))
(defmethod set-pop ((item-set item-set))
  (with-slots (memberlist) item-set
    (pop memberlist)))

(defgeneric set-empty (item-set)
  (:documentation "Predicate."))
(defmethod set-empty ((item-set item-set))
  (with-slots (memberlist) item-set
    (null memberlist)))

;;===================================================================

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

(defclass bfs ()
  ((open-set   :initarg :open-set   :initform (make-instance 'queue))
   (closed-set :initarg :closed-set :initform (make-instance 'item-set))
   (verbose    :initarg :verbose    :initform nil :accessor bfs-verbose)))

(defgeneric children-of (bfs id)
  (:documentation "Return the ids of the children of the given node"))
(defmethod children-of ((bfs bfs) id)
  (format t "have to override this one~%")
  (sb-ext:exit :code 1))
        
(defgeneric next-best (bfs)
  (:documentation "Return the next best member of the open-set"))
(defmethod next-best ((bfs bfs))
  (with-slots (open-set) bfs
    (dequeue  open-set)))

(defgeneric add-children-to-frontier (bfs parent)
  (:documentation "duh"))
(defmethod add-children-to-frontier ((bfs bfs) parent)
  (with-slots (open-set closed-set verbose) bfs
    (if verbose
	(format t "adding children of ~a~%" parent))
    (dolist (child (children-of bfs parent))
      (if verbose
	(format t "  processing child ~a~%" child))
      (if (set-contains closed-set child)
	  (if verbose
	      (format t  "   already processed~%"))
	  (if (contains open-set child)
	      (if verbose
		  (format t  "   already in open-set~%"))
	      (enqueue open-set child))))))

(defgeneric visit-all (bfs startnode func funcarg)
  (:documentation "Call (func item arg) on each member of the open-set, bfs style"))
(defmethod visit-all ((bfs bfs) startnode func funcarg)
  (with-slots (open-set closed-set verbose) bfs
    (enqueue open-set startnode)
    (do ()
	((empty open-set))
      (let ((node (next-best bfs)))
	(if verbose
	    (format t "VISITING node ~a~%" node))
	(funcall func node funcarg)
	(if verbose
	    (format t "VISIT complete~%"))
	;; here is where the test for completion goes
	(add-children-to-frontier bfs node)
	(set-add closed-set node)))))

(defclass prog-bfs (bfs)
  ((nodes :initarg :nodes :initform nil)))

(defgeneric children-of (prog-bfs id)
  (:documentation "Return the ids of the children of the given node"))
(defmethod children-of ((bfs prog-bfs) id)
  (with-slots (nodes) bfs
    (gethash 'neighbors (gethash id nodes))))
    
;; ==========================================
(defun prtnode (n v)
  (vector-push-extend n v))
  
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
	  (visit-all srch goal #'prtnode s1)
	  (format t "node ~a can reach ~a others~%" goal (1- (length s1)))
	  (setf sleft (set-difference sleft (concatenate 'list s1)))))))
  0)

(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
