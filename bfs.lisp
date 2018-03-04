(defpackage :dls
  (:use :common-lisp)
  (:export :bfs :children-of :next-best :visit-all))

(in-package :dls)

(load "queues.lisp")
(load "sets.lisp")

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
    (dequeue open-set)))

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
  (:documentation "Call (func item arg) on each member of the open-set, bfs style. Stop if func returns non-nil."))
(defmethod visit-all ((bfs bfs) startnode func funcarg)
  (with-slots (open-set closed-set verbose) bfs
    (enqueue open-set startnode)
    (do ()
	((empty open-set))
      (let ((node (next-best bfs)))
	(let ((val (funcall func node funcarg)))
	  (when val
	    (return-from visit-all val)))
	;; here is where the test for completion goes
	(add-children-to-frontier bfs node)
	(set-add closed-set node))))
  nil)
